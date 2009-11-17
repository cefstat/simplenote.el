;;  simplenote.el --- Interact with simple-note.appspot.com

;; Copyright (C) 2009 Konstantinos Efstathiou <konstantinos@efstathiou.gr>

;; Author: Konstantinos Efstathiou <konstantinos@efstathiou.gr>
;; Keywords: simplenote

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 51
;; Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.


;;; Code:



(require 'cl)
(require 'url)
(require 'json)
(require 'widget)

(defcustom simplenote-directory (expand-file-name "~/.simplenote/")
  "Simplenote directory."
  :safe t
  :group 'simplenote)

(defcustom simplenote-email nil
  "Simplenote account email."
  :safe t
  :group 'simplenote)

(defcustom simplenote-password nil
  "Simplenote account password."
  :safe t
  :group 'simplenote)

(defface simplenote-browse-line-highlight
  '((default :background "orange" :weight "bold"))
  "Face for highlighting the current line."
  :group 'simplenote)

(make-variable-buffer-local 'simplenote-key)



;;; Simplenote authentication

(defvar simplenote-token nil)
(defvar simplenote-token-set-at nil)
(defvar simplenote-email-was-read-interactively nil)
(defvar simplenote-password-was-read-interactively nil)

(defun simplenote-email ()
  (when (not simplenote-email)
    (setq simplenote-email (read-string "Simplenote email: "))
    (setq simplenote-email-was-read-interactively t))
  simplenote-email)

(defun simplenote-password ()
  (when (not simplenote-password)
    (setq simplenote-password (read-passwd "Simplenote password: "))
    (setq simplenote-password-was-read-interactively t))
  simplenote-password)

(defun simplenote-get-token (email password)
  (let (url url-request-method url-request-extra-headers url-request-data token)
    (setq url "https://simple-note.appspot.com/api/login")
    (setq url-request-method "POST")
    (setq url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
    (setq url-request-data
          (base64-encode-string (format "email=%s&password=%s"
                                        (url-hexify-string email)
                                        (url-hexify-string password))))
    (with-current-buffer (url-retrieve-synchronously url)
      (setq status url-http-response-status)
      (when (eql status 200)
        (goto-char (point-min))
        (search-forward-regexp "^$" nil t)
        (setq token (buffer-substring (1+ (point)) (point-max)))))
    token))

(defun simplenote-token ()
  (interactive)
  (if (or (not simplenote-token)
          (> (time-to-seconds (time-subtract (current-time) simplenote-token-set-at))
             86400))
      (let ((token (simplenote-get-token (simplenote-email) (simplenote-password))))
        (if token
            (progn
              (setq simplenote-token token)
              (setq simplenote-token-set-at (current-time))
              (message "Simplenote authentication succeeded"))
          (progn
            (if simplenote-email-was-read-interactively
                (setq simplenote-email nil))
            (if simplenote-password-was-read-interactively
                (setq simplenote-password nil))
            (setq simplenote-token nil)
            (setq simplenote-token-set-at nil)
            (message "Simplenote authentication failed")))))
  simplenote-token)


;;; API calls for index and notes

(defun simplenote-get-index (token email)
  (let (url status headers data index)
    (setq url (format "https://simple-note.appspot.com/api/index?auth=%s&email=%s"
                      (url-hexify-string token)
                      (url-hexify-string email)))
    (with-current-buffer (url-retrieve-synchronously url)
      (setq status url-http-response-status)
      (when (eql status 200)
        (goto-char (point-min))
        (search-forward-regexp "^$" nil t)
        (setq headers (buffer-substring (point-min) (point)))
        (setq data (buffer-substring (1+ (point)) (point-max)))
        (setq index (json-read-from-string data))))
    index))

(defun simplenote-get-note (key token email)
  (let (url status headers data note-key note-modifydate note-createdate note-deleted)
    (setq url (format
               "https://simple-note.appspot.com/api/note?key=%s&auth=%s&email=%s"
               (url-hexify-string key)
               (url-hexify-string token)
               (url-hexify-string email)))
    (with-current-buffer (url-retrieve-synchronously url)
      (setq status url-http-response-status)
      (when (eql status 200)
        (goto-char (point-min))
        (search-forward-regexp "^$" nil t)
        (setq headers (buffer-substring (point-min) (point)))
        (setq data (decode-coding-string
                    (buffer-substring (1+ (point)) (point-max))
                    'utf-8))
        (string-match "^note-key: \\(.*\\)$" headers)
        (setq note-key (match-string 1 headers))
        (string-match "^note-modifydate: \\(.*\\)$" headers)
        (setq note-modifydate (date-to-time (match-string 1 headers)))
        (string-match "^note-createdate: \\(.*\\)$" headers)
        (setq note-createdate (date-to-time (match-string 1 headers)))
        (string-match "^note-deleted: \\(.*\\)$" headers)
        (setq note-deleted (match-string 1 headers))))
    (values data note-key note-createdate note-modifydate note-deleted)))

(defun simplenote-mark-note-as-deleted (key token email)
  (let (url)
    (setq url (format
               "https://simple-note.appspot.com/api/delete?key=%s&auth=%s&email=%s"
               (url-hexify-string key)
               (url-hexify-string token)
               (url-hexify-string email)))
    (with-current-buffer (url-retrieve-synchronously url)
      (eql url-http-response-status 200))))

(defun simplenote-update-note (key text token email &optional modifydate)
  (let (url url-request-method url-request-data status note-key)
    (if modifydate
        (setq url (format
                   "https://simple-note.appspot.com/api/note?key=%s&auth=%s&email=%s&modify=%s"
                   (url-hexify-string key)
                   (url-hexify-string token)
                   (url-hexify-string email)
                   (url-hexify-string (format-time-string "%Y-%m-%d %H:%M:%S" modifydate))))
      (setq url (format
                 "https://simple-note.appspot.com/api/note?key=%s&auth=%s&email=%s"
                 (url-hexify-string key)
                 (url-hexify-string token)
                 (url-hexify-string email))))
    (setq url-request-method "POST")
    (setq url-request-data (base64-encode-string text))
    (with-current-buffer (url-retrieve-synchronously url)
      (setq status url-http-response-status)
      (when (eql status 200)
        (goto-char (point-min))
        (search-forward-regexp "^$" nil t)
        (setq note-key (buffer-substring (1+ (point)) (point-max)))))
    note-key))

(defun simplenote-create-note (text token email &optional createdate)
  (let (url url-request-method url-request-data status headers note-key)
    (if createdate
        (setq url (format
                   "https://simple-note.appspot.com/api/note?auth=%s&email=%s&create=%s"
                   (url-hexify-string token)
                   (url-hexify-string email)
                   (url-hexify-string (format-time-string "%Y-%m-%d %H:%M:%S" createdate))))
      (setq url (format
                 "https://simple-note.appspot.com/api/note?auth=%s&email=%s"
                 (url-hexify-string token)
                 (url-hexify-string email))))
    (setq url-request-method "POST")
    (setq url-request-data (base64-encode-string text))
    (with-current-buffer (url-retrieve-synchronously url)
      (setq status url-http-response-status)
      (when (eql status 200)
        (goto-char (point-min))
        (search-forward-regexp "^$" nil t)
        (setq note-key (buffer-substring (1+ (point)) (point-max)))))
    note-key))


;;; Push and pull buffer as note

(defun simplenote-file-mtime-gmt (path)
  (let (mtime tz-offset)
    (setq mtime (nth 5 (file-attributes path)))
    (setq tz-offset (nth 8 (decode-time mtime)))
    (time-add mtime (butlast (seconds-to-time (- tz-offset))))))

(defun simplenote-push-buffer ()
  (interactive)
  (let (modifydate success)
    (save-buffer)
    (setq modifydate (simplenote-file-mtime-gmt (buffer-file-name)))
    (setq success (simplenote-update-note simplenote-key
                                          (encode-coding-string (buffer-string) 'utf-8)
                                          (simplenote-token)
                                          (simplenote-email)
                                          modifydate))
    (if success
        (message "Pushed note %s" simplenote-key)
      (message "Failed to push note %s" simplenote-key))))

(defun simplenote-create-note-from-buffer ()
  (interactive)
  (let (createdate key)
    (save-buffer)
    (setq createdate (simplenote-file-mtime-gmt (buffer-file-name)))
    (setq key (simplenote-create-note (encode-coding-string (buffer-string) 'utf-8)
                                      (simplenote-token)
                                      (simplenote-email)
                                      createdate))
    (if key
        (progn
          (setq simplenote-key key)
          (message "Created note %s" key))
      (message "Failed to create new note"))))

(defun simplenote-pull-buffer ()
  (interactive)
  (multiple-value-bind (data note-key note-createdate note-modifydate note-deleted)
      (simplenote-get-note simplenote-key
                           (simplenote-token)
                           (simplenote-email))
    (if data
        (progn
          (erase-buffer)
          (insert data)
          (message "Pulled note %s" simplenote-key))
      (message "Failed to pull note %s" simplenote-key))))


;;; Simplenote sync

(defun simplenote-sync-notes ()
  (interactive)
  (if (not (file-exists-p simplenote-directory))
      (make-directory simplenote-directory t))
  (let (index files files-marked-deleted)
    (setq files (directory-files simplenote-directory t "^[a-zA-Z0-9_\\-]\\{36\\}$"))
    (setq files-marked-deleted (directory-files simplenote-directory t
                                                "^[a-zA-Z0-9_\\-]\\{36\\}-$"))
    (setq index (simplenote-get-index (simplenote-token) (simplenote-email)))
    (if (not index)
        (message "Could not retrieve the index")
      ;; For each note in the index that is not deleted
      (loop for elem across index do
            (let (key deleted modify path path-del note-text note-key temp-buffer)
              (setq key (cdr (assoc 'key elem)))
              (setq deleted (eq (cdr (assoc 'deleted elem)) t))
              (setq modify (date-to-time (cdr (assoc 'modify elem))))
              (setq path (concat (file-name-as-directory simplenote-directory) key))
              (when (not deleted)
                ;; Download
                (when (or (not (file-exists-p path))
                          (time-less-p (nth 5 (file-attributes path)) modify))
                  (message "Downloading note %s from Simplenote" key)
                  (multiple-value-bind (note-text note-key note-createdate
                                                  note-modifydate note-deleted)
                      (simplenote-get-note key (simplenote-token) (simplenote-email))
                    (if note-text
                        (progn
                          (message "Downloaded note %s" key)
                          (write-region note-text nil path nil)
                          (set-file-times path note-modifydate))
                      (message "Failed to download note %s" key))))
                ;; Upload
                (when (and (file-exists-p path)
                           (time-less-p modify (nth 5 (file-attributes path))))
                  (message "Uploading note %s to Simplenote" key)
                  (setq temp-buffer (get-buffer-create " *simplenote-temp*"))
                  (with-current-buffer temp-buffer
                    (insert-file-contents path nil nil nil t)
                    (setq note-text (encode-coding-string (buffer-string) 'utf-8)))
                  (kill-buffer " *simplenote-temp*")
                  (setq note-key (simplenote-update-note key
                                                         note-text
                                                         (simplenote-token)
                                                         (simplenote-email)
                                                         (simplenote-file-mtime-gmt path)))
                  (if note-key
                      (message "Uploaded note %s" note-key)
                    (message "Failed to upload note %s" note-key)))
                ;; Remove the note from the list of files to delete
                (setq files (delete path files)))))
      
      ;; If the file has an ID but is not in the index or is in the index but has
      ;; been marked as deleted,then delete it from the file system
      (loop for file in files do
            (message "Deleting file %s" file)
            (delete-file file)))
    
    ;; If a file has been marked locally as deleted then sync the deletion and
    ;; delete from the file system
    (loop for file in files-marked-deleted do
          (let (key success)
            (setq key (substring (file-name-nondirectory file) 0 -1))
            (setq success (simplenote-mark-note-as-deleted key
                                                           (simplenote-token)
                                                           (simplenote-email)))
            (when success
              (message "Deleting file %s" file)
              (delete-file file))))
    
    ;; If a new file has been locally created then create a new note and rename
    ;; the local file
    (loop for file in (directory-files simplenote-directory t "new_[0-9]+") do
          (let (temp-buffer text note-key mod-time)
            (setq temp-buffer (get-buffer-create " *simplenote-temp*"))
            (with-current-buffer temp-buffer
              (insert-file-contents file nil nil nil t)
              (setq text (encode-coding-string (buffer-string) 'utf-8)))
            (kill-buffer " *simplenote-temp*")
            (setq mod-time (nth 5 (file-attributes file)))
            (setq note-key (simplenote-create-note text
                                                   (simplenote-token)
                                                   (simplenote-email)
                                                   (simplenote-file-mtime-gmt file)))
            (when note-key
              (let (new-filename)
                (setq new-filename (concat (file-name-as-directory simplenote-directory) note-key))
                (rename-file file new-filename)
                (set-file-times new-filename mod-time))
              )))

    ;; Refresh the browser
    (let (sn-browse-buf)
      (setq sn-browse-buf (get-buffer "*Simplenote*"))
      (if sn-browse-buf
          (with-current-buffer sn-browse-buf
            (simplenote-browser-refresh))))
    ))



;;; Simplenote browse local notes

(defun simplenote-browse ()
  (interactive)
  (switch-to-buffer "*Simplenote*")
  (setq buffer-read-only t)
  (simplenote-browse-mode)
  (simplenote-menu-setup))

(defun simplenote-browser-refresh ()
  (interactive)
  (simplenote-browse))

(defun simplenote-browse-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'simplenote-browse-mode)
  (setq mode-name "Simplenote"))

(defun simplenote-menu-setup ()
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-create 'link
                 :format "%[%v%]"
                 :notify (lambda (widget &rest ignore)
                           (simplenote-create-note-locally)
                           (simplenote-browser-refresh))
                 "Create new note")
  (widget-insert "\n\n")
  (let (files lines)
    (setq files (directory-files simplenote-directory nil "^\\(new_.*\\|[a-zA-Z0-9_\\-]\\{36\\}-?\\)$"))
    (setq files (sort files 'simplenote-note-newer-p))
    (setq lines (mapcar 'simplenote-note-widget files))
    (widget-create 'group :value lines))
  (use-local-map widget-keymap)
  (widget-setup))

(defun simplenote-note-newer-p (key1 key2)
  (let (time1 time2)
    (setq time1 (nth 5 (file-attributes (concat (file-name-as-directory simplenote-directory) key1))))
    (setq time2 (nth 5 (file-attributes (concat (file-name-as-directory simplenote-directory) key2))))
    (time-less-p time2 time1)))

(defun simplenote-note-widget (key)
  (let (file modify modify-string note-text note-short temp-buffer
             delete-me undelete-me)
    (setq file (concat (file-name-as-directory simplenote-directory) key))
    (setq modify (nth 5 (file-attributes file)))
    (setq modify-string (format-time-string "%Y-%m-%d %H:%M:%S" modify))
    (setq delete-me (lambda (widget &rest ignore)
                      (simplenote-mark-note-for-deletion (widget-get widget :tag))
                      (widget-put widget :notify undelete-me)
                      (widget-value-set widget "Undelete")
                      (widget-setup)))
    (setq undelete-me (lambda (widget &rest ignore)
                        (simplenote-unmark-note-for-deletion (widget-get widget :tag))
                        (widget-put widget :notify delete-me)
                        (widget-value-set widget "Delete")
                        (widget-setup)))
    (setq temp-buffer (get-buffer-create " *simplenote-temp*"))
    (with-current-buffer temp-buffer
      (insert-file-contents file nil nil nil t)
      (setq note-text (encode-coding-string (buffer-string) 'utf-8)))
    (kill-buffer " *simplenote-temp*")
    (setq note-short
          (replace-regexp-in-string "\n" " "
                                    (substring note-text 0 (min 78 (length note-text)))))
    (widget-create 'link
                   :button-prefix ""
                   :button-suffix ""
                   :format "%[%v%]\n"
                   :tag file
                   :notify (lambda (widget &rest ignore)
                             (find-file (widget-get widget :tag)))
                   note-short)
    (widget-insert (format "%s\t%s\n" (substring key 0 36) modify-string))
    (widget-create 'link
                   :format "%[%v%]"
                   :tag (substring key 0 36)
                   :notify delete-me
                   "Delete")
    (widget-insert "\n\n")
))

(defun simplenote-mark-note-for-deletion (key)
  (rename-file (concat (file-name-as-directory simplenote-directory) key)
               (concat (file-name-as-directory simplenote-directory) key "-")))

(defun simplenote-unmark-note-for-deletion (key)
  (rename-file (concat (file-name-as-directory simplenote-directory) key "-")
               (concat (file-name-as-directory simplenote-directory) key)))

(defun simplenote-create-note-locally ()
  )


(provide 'simplenote)

;;; simplenote.el ends here

