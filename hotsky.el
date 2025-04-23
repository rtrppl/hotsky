;;; hotsky.el --- Fetch current links from Bluesky timeline -*- lexical-binding: t; -*-

;; Maintainer: René Trappel <rtrappel@gmail.com>
;; URL: https://github.com/rtrppl/hotsky
;; Version: 0.1.2
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; hotsky.el provides an Orgmode buffer with a sorted list of links
;; from posts in your Bluesky feed.

;;; News
;;
;; 0.1.2
;; - Added for g=refresh

;;; Code:

(require 'json)
(require 'xml)
(require 'org)

(defvar hotsky-bluesky-handle "handle.bsky.social")
(defvar hotsky-bluesky-app-password "bluesky-password")
(defvar hotsky-accessJwt) ;;access token for Bluesky
(defvar hotsky-max-posts 250)

(defun hotsky-bluesky-authenticate ()
  "Authenticate with Bluesky."
  (let ((bluesky-callback-json))
  (setq bluesky-callback-json (shell-command-to-string
			  (concat "curl -sS -X POST \\\n"
				  "-H \"Content-Type: application/json\" \\\n"
				  "-d '{\"identifier\":\"" hotsky-bluesky-handle "\",\"password\":\"" hotsky-bluesky-app-password "\"}' \\\n"
				  "\"https://bsky.social/xrpc/com.atproto.server.createSession\"")))
  (setq hotsky-accessJwt (json-parse-string bluesky-callback-json :object-type 'alist))
  (setq hotsky-accessJwt (alist-get 'accessJwt hotsky-accessJwt))))
  
(defun hotsky-get-timeline (&optional max-posts)
  "Load up to MAX-POSTS entries from the Bluesky timeline."
  (hotsky-bluesky-authenticate)
  (let* ((max-posts (or max-posts 100))
         (url "https://bsky.social/xrpc/app.bsky.feed.getTimeline")
         (auth (concat "Authorization: Bearer " hotsky-accessJwt))
         (limit 100)
         (cursor nil)
         (posts '())
         (done nil)
         (round 0))
    (while (and (not done) (< (length posts) max-posts))
      (setq round (1+ round))
      (message "🔄 Round %d: Fetched %d/%d posts so far..." round (length posts) max-posts)
      (let* ((curl-cmd
              (concat "curl -sS -G"
                      " -H \"" auth "\""
                      " --data-urlencode \"limit=" (number-to-string limit) "\""
                      (if cursor
                          (concat " --data-urlencode \"cursor=" cursor "\"")
                        "")
                      " \"" url "\""))
             (raw (shell-command-to-string curl-cmd)))
        (condition-case err
            (let* ((json (json-parse-string raw :object-type 'hash-table))
                   (feed (gethash "feed" json))
                   (next-cursor (gethash "cursor" json)))
              ;; Append feed
              (setq posts (append posts (append feed nil)))
              (if (or (not next-cursor)
                      (>= (length posts) max-posts))
                  (setq done t)
                (setq cursor next-cursor)))
          (error (message "❌ JSON parse error: %s" err)
                 (setq done t)))))
    (let ((final (cl-subseq posts 0 (min (length posts) max-posts))))
      (message "✅ Done: Fetched total %d posts" (length final))
      final)))

(defun hotsky--extract-urls (record)
  "Helper to extract URLs from a post RECORD."
  (let ((facets (and (hash-table-p record) (gethash "facets" record)))
        (urls '()))
    (when facets
      (mapc (lambda (facet)
              (let ((features (gethash "features" facet)))
                (mapc (lambda (feature)
                        (when (string= (gethash "$type" feature) "app.bsky.richtext.facet#link")
                          (let ((url (replace-regexp-in-string "\\?.*" "" (gethash "uri" feature))))
                            (push url urls))))
                      features)))
            facets))
    urls))

(defun hotsky-get-cid-url-map ()
  "Return a hashtable mapping cid to a list of all URLs in each post."
  (let* ((timeline (hotsky-get-timeline hotsky-max-posts)) ;; Optionally raise count
         (result (make-hash-table :test #'equal)))
    (mapc (lambda (entry)
            (let* ((post (gethash "post" entry))
                   (cid (and (hash-table-p post) (gethash "cid" post)))
                   (all-urls '()))

              ;; URLs in main post
              (when (hash-table-p post)
                (let ((record (gethash "record" post)))
                  (when (hash-table-p record)
                    (setq all-urls (append (hotsky--extract-urls record) all-urls)))))

              ;; URLs in repost
              (let ((reason (gethash "reason" entry)))
                (when (and (hash-table-p reason)
                           (string= (gethash "$type" reason) "app.bsky.feed.defs#reasonRepost"))
                  (let* ((repost-post (gethash "post" reason))
                         (record (and (hash-table-p repost-post) (gethash "record" repost-post))))
                    (when (hash-table-p record)
                      (setq all-urls (append (hotsky--extract-urls record) all-urls))))))

              ;; URLs in quote post
              (when (hash-table-p post)
                (let* ((record (gethash "record" post))
                       (embed (and (hash-table-p record) (gethash "embed" record)))
                       (quoted-record (and (hash-table-p embed)
                                           (gethash "record" embed))))
                  (when (hash-table-p quoted-record)
                    (setq all-urls (append (hotsky--extract-urls quoted-record) all-urls)))))

              ;; Save if we have any URLs
              (when (and cid all-urls)
                (puthash cid (reverse all-urls) result))))
          timeline)
    result))

(defun hotsky-get-all-urls ()
  "Return a list with all URLs."
  (let* ((cid-url-map (hotsky-get-cid-url-map))
	 (all-cids (hash-table-keys cid-url-map))
	 (all-urls '()))
    (dolist (entry all-cids)
      (let* ((url (gethash entry cid-url-map)))
	(if (stringp url)
	    (push url all-urls)
	  (dolist (item url)
	    (push item all-urls)))))
    all-urls))

(defun hotsky-get-url-name-map (all-urls)
  "Return a hashtable mapping the names of websites to ALL-URLS."
  (let* ((url-name-map (make-hash-table :test #'equal)))
    (dolist (entry all-urls)
      (message "Fetching name for %s." entry)
      (puthash entry (hotsky-get-name-for-url entry) url-name-map))
    (message "All names are fetched.")
    url-name-map))
		 
(defun hotsky-get-name-for-url (url)
  "Return the name of website via cURL from URL."
  (let* ((cmd "curl -L -s ")
	 (hotsky-max-length-entry (- (window-body-width) 11))
         (title nil)
         (title-p nil))
    (with-temp-buffer
      (insert (shell-command-to-string (concat cmd url)))
      (goto-char (point-min))
      ;; Try to extract the title contents
      (while (re-search-forward "<title[^>]*>\\([^>]*\\)</title>" nil t)
        (when (and (not title-p)
		   (match-string 0))
          (setq title (match-string 1))
          (setq title-p t))))
    ;; Only clean up the title if it's not nil
    (when title
      (setq title (replace-regexp-in-string "[ \n\t]" " " title))
      (setq title (xml-substitute-special title)))
    (when (or (string= "Just a moment..." title)
	      (string= "Access to this page has been denied" title)
	      (string= "reuters.com" title)
	      (string= "Access Denied" title)
	      (string= "ERROR: The request could not be satisfied" title)
	      (string= "Telegram" title)
	      (string= "Bloomberg - Are you a robot?" title)
	      (string= "Subscribe to read" title))
      (setq title nil))
    (when (> (length url) hotsky-max-length-entry)
      (setq url (concat (substring url 0 hotsky-max-length-entry) "...")))
    (when (> (length title) hotsky-max-length-entry)
      (setq title (concat (substring title 0 hotsky-max-length-entry) "...")))
    ;; Fallback: if title not found, return the URL
    (or title url)))

(defun hotsky-sort-hot (all-urls)
  "Sort the URLs in ALL-URLS for frequency."
  (let* ((url-frequency-map (make-hash-table :test #'equal))
         (url-list '()))
    ;; Count frequencies of each URL
    (dolist (url all-urls)
      (let ((count (gethash url url-frequency-map 0))) ; Default count is 0 if not found
        (puthash url (1+ count) url-frequency-map)))
    ;; Convert the hash table to a list of (URL . frequency) pairs
    (maphash (lambda (url count)
               (push (cons url count) url-list))
             url-frequency-map)
    (setq url-list (nreverse url-list))
    ;; Sort the list by frequency (descending order)
    (sort url-list (lambda (a b) (> (cdr a) (cdr b))))))

(defun hotsky ()
  "Return a sorted list of all URLs in my Bluesky feed."
  (interactive)
  (let ((hotsky-buffer "*hotsky buffer*"))
    (with-current-buffer (get-buffer-create hotsky-buffer)
      (erase-buffer)
      (switch-to-buffer hotsky-buffer)
      (org-mode)
      (insert "* Links in your Bluesky timeline sorted by frequency (No. of ▌) and time posted. \n\n")
      (insert "(q=kill this buffer, g=refresh, n=next link, p=previous link, <=goto first link, >=goto last link, return=open link, space=scroll)\n\n")
      (let* ((all-urls (hotsky-get-all-urls))
             (sorted-list (hotsky-sort-hot all-urls))
             (url-name-map (hotsky-get-url-name-map all-urls)))
	(message "Done.")
	(dolist (item sorted-list)
	  (when (not (string= "https://www.youtube.com/watch" (car item)))
	    (let* ((name (gethash (car item) url-name-map))
		   (frequency (concat (make-string (cdr item) ?▌)))
		   (org-link (concat "[[" (car item) "][" name "]]"))
		   (full-line (concat frequency " " org-link "\n")))
              (insert full-line))))
	(goto-char (point-min))
	(org-next-link)
	(hotsky-buffer-mode)))))
 
(defun hotsky-org-goto-first-link ()
  "Move point to the first link in the current buffer."
  (interactive)
  (goto-char (point-min))
  (org-next-link))

(defun hotsky-org-goto-last-link ()
  "Move point to the last link in the current buffer."
  (interactive)
  (goto-char (point-max))
  (org-previous-link))

(define-minor-mode hotsky-buffer-mode
  "A minor mode for the hotsky buffer."
  :lighter " hotsky-buffer"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") (lambda () (interactive) (kill-buffer (current-buffer))))
	    (define-key map (kbd "g") 'hotsky)
	    (define-key map (kbd "n") 'org-next-link)
	    (define-key map (kbd "p") 'org-previous-link)
	    (define-key map (kbd "<") 'hotsky-org-goto-first-link)
	    (define-key map (kbd ">") 'hotsky-org-goto-last-link)
	    (define-key map (kbd "<return>") 'org-open-at-point)
	    (define-key map (kbd "SPC") 'scroll-up-command)
            map))
    
(provide 'hotsky)

;;; hotsky.el ends here
