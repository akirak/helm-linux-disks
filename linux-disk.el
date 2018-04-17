;;; linux-disk.el --- Operations on removable volumes in Linux -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))
;; URL: https://github.com/akirak/helm-linux-disks

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library defines `linux-disk' CL object as well as a bunch of operations
;; on the object type. `linux-disk' object holds information on a block device
;; which can be retrieved by lsblk command.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; This object holds information on each record in the output from lsblk command.
;; See helm-linux-disks.el on parsing.
(cl-defstruct linux-disk path mountpoint fstype type size has-child-p)

;;;; Predicates

(defun linux-disk-mounted-p (struct)
  "Return non-nil if STRUCT is currently mounted."
  (linux-disk-mountpoint struct))

(defun linux-disk-mountable-p (struct)
  "Return non-nil if STRUCT is a mountable device."
  (or (eq (linux-disk-type struct) 'lvm)
      (and (eq (linux-disk-type struct) 'part)
           (not (linux-disk-luks-p struct)))))

(defun linux-disk-crypt-target-p (struct)
  "Return non-nil if STRUCT is an open dm-crypt target."
  (eq (linux-disk-type struct) 'crypt))

(defun linux-disk-luks-p (struct)
  "Return non-nil if STRUCT is a LUKS volume."
  (string-equal (linux-disk-fstype struct) "crypto_LUKS"))

(defun linux-disk-luks-close-p (struct)
  "Return non-nil if STRUCT is a closed LUKS volume. "
  (and (linux-disk-luks-p struct)
       (not (linux-disk-has-child-p struct))))

(defun linux-disk-luks-open-p (struct)
  "Return non-nil if STRUCT is an open LUKS volume."
  (and (linux-disk-luks-p struct)
       (linux-disk-has-child-p struct)))

(defun linux-disk-disk-p (struct)
  "Return non-nil if STRUCT is a normal disk device."
  (eq (linux-disk-type struct) 'disk))

;;;; Operations

(defun linux-disk-dwim (struct)
  "Mount (unlock) or unmount (lock) the device depending on the state of STRUCT.

STRUCT should be a `linux-disk' object. If the argument represents an unmounted
(locked) device, it is mounted (unlocked). If the argument represents a mounted
(unlocked) device, it is unmounted (locked)."
  (cond
   ((linux-disk-mounted-p struct) (linux-disk-unmount struct))
   ((linux-disk-mountable-p struct) (linux-disk-mount struct))
   ((linux-disk-crypt-target-p struct) (linux-disk-cryptsetup-luks-close struct))
   ((linux-disk-luks-close-p struct) (linux-disk-udisksctl-unlock struct))
   ((linux-disk-luks-p struct) (linux-disk-udisksctl-lock struct))
   ((linux-disk-disk-p struct) (linux-disk-udisksctl-poweroff struct))
   (t (message "no idea what to do"))))

(defun linux-disk-mount (struct)
  "Mount a file system volume.

STRUCT must be a `linux-disk' object representing an unmounted file system
object."
  (unless (linux-disk-mountable-p struct)
    (error "not mountable"))
  (cond
   ((executable-find "udisksctl") (linux-disk-udisksctl-mount struct))
   ;; TODO: mount using mount
   (t (error "udisksctl is not available"))))

(defun linux-disk-unmount (struct)
  "Unmount a file system volume.

STRUCT must be a `linux-disk' object representing an mounted file system object."
  (unless (linux-disk-mounted-p struct)
    (error "not mounted"))
  (cond
   ((executable-find "udisksctl") (linux-disk-udisksctl-unmount struct))
   ;; TODO: unmount using umount
   (t (error "udisksctl is not available"))))

;;;;; Operations using udisksctl

(defun linux-disk-udisksctl-mount (struct)
  "Mount a file system volume using udisksctl command.

STRUCT must be a `linux-disk' object representing an unmounted file system
object."
  (unless (linux-disk-mountable-p struct)
    (error "not mountable"))
  (let ((path (linux-disk-path struct)))
    (message "mounting %s using udisksctl..." path)
    (start-process "udisksctl-mount" nil
                   "udisksctl" "mount" "--block-device" path)))

(defun linux-disk-udisksctl-unmount (struct)
  "Unmount a file system volume using udisksctl command.

STRUCT must be a `linux-disk' object representing a mounted file system object."
  (unless (linux-disk-mounted-p struct)
    (error "not mounted"))
  (if (linux-disk-ensure-unmountable (linux-disk-mountpoint struct))
      (let ((path (linux-disk-path struct)))
        (message "unmounting %s using udisksctl..." path)
        (start-process "udisksctl-unmount" nil
                       "udisksctl" "unmount" "--block-device" path))
    (message "Not unmountable")))

(defun linux-disk-udisksctl-lock (struct)
  "Lock a crypted device using udisksctl command.

STRUCT must be a `linux-disk' object representing a closed LUKS device."
  (unless (linux-disk-luks-open-p struct)
    (error "not a decryption device"))
  (let ((path (linux-disk-path struct)))
    (message "locking %s..." path)
    (start-process "udisksctl-lock" nil
                   "udisksctl" "lock" "--block-device" path)))

(defun linux-disk-udisksctl-unlock (struct)
  "Unlock a crypted device using udisksctl command.

STRUCT must be a `linux-disk' object representing an open LUKS device."
  (unless (linux-disk-luks-close-p struct)
    (error "not a LUKS device"))
  (let ((path (linux-disk-path struct)))
    (message "unlocking %s..." path)
    (eshell-command
     (string-join (list "udisksctl"
                        "unlock"
                        "--block-device"
                        (shell-quote-argument path))
                  " "))))

(defun linux-disk-udisksctl-info (struct)
  "Display information about a device retrieved by udisksctl command.

STRUCT should be a `linux-disk' object on any block device that can be handled
by udisks."
  (with-current-buffer (generate-new-buffer "*udisksctl info*")
    (insert (shell-command-to-string
             (string-join (list "udisksctl"
                                "info"
                                "--block-device"
                                (shell-quote-argument (linux-disk-path struct)))
                          " ")))
    (local-set-key (kbd "q") #'quit-window)
    (read-only-mode 1)
    (pop-to-buffer (current-buffer))))

(defun linux-disk-udisksctl-poweroff (struct)
  "Power off a device using udisksctl command.

STRUCT should be a `linux-disk' object representing a physical device. This is
normally the entire device such as /dev/sdb."
  (let ((path (linux-disk-path struct)))
    (when (yes-or-no-p (format "Are you sure you want to power off %s? "
                               path))
      (start-process "udisksctl-poweroff" nil
                     "udisksctl" "power-off" "--block-device" path))))

;;;;; Operations using cryptsetup

(defun linux-disk-cryptsetup-luks-close (struct)
  "Close (lock) a dm-crypt target using cryptsetup command.

STRUCT should be a `linux-disk' object representing a physical device. This
normally has a dynamically mapped path like /dev/mapper/luks-XXXX."
  (unless (linux-disk-crypt-target-p struct)
    (error "not a decryption device"))
  (let ((path (linux-disk-path struct)))
    (message "locking %s..." path)
    (start-process "luks-close" nil
                   "sudo" "cryptsetup" "luksClose" path)))

;;;;; Running dired on the mount point

(defun linux-disk-dired (struct)
  "Run dired on the mount point of STRUCT.

STRUCT should be a `linux-disk' object representing a mounted device. It also
needs to contain information on the mount point."
  (let ((mountpoint (linux-disk-mountpoint struct)))
    (unless mountpoint
      (error "not mounted"))
    (dired mountpoint)))

(defun linux-disk-dired-other-window (struct)
  "Run dired on the mount point of STRUCT in another window.

STRUCT should be a `linux-disk' object representing a mounted device. It also
needs to contain information on the mount point."
  (let ((mountpoint (linux-disk-mountpoint struct)))
    (unless mountpoint
      (error "not mounted"))
    (dired-other-window mountpoint)))

;;;;; Running terminal on the mount point

(defun linux-disk-terminal (struct)
  "Open a terminal on the mount point of STRUCT.

STRUCT should be a `linux-disk' object representing a mounted device. It also
needs to contain information on the mount point."
  (let ((mountpoint (linux-disk-mountpoint struct)))
    (unless mountpoint
      (error "not mounted"))
    (let ((default-directory mountpoint))
      (pcase linux-disk-terminal-type
        ('term (call-interactively 'term))
        ('ansi-term (call-interactively 'ansi-term))
        ('multi-term (multi-term))
        ('eshell (eshell t))
        ((pred stringp) (async-shell-command linux-disk-terminal-type))))))

(defcustom linux-disk-terminal-type 'term
  "Type of terminal invoked by `linux-disk-terminal' command.

If a string is given as the value of this variable, it is run as a terminal
 program."
  :type '(choice (const term)
                 (const ansi-term)
                 (const multi-term)
                 (const eshell)
                 string)
  :group '(linux-disk helm-linux-disks))

;;;; Other utility functions

(defun linux-disk-ensure-unmountable (mountpoint)
  "Return non-nil if a device at MOUNTPOINT is ready to unmount.

A device is ready to unmount if it satisfies the following conditions:

- No Emacs buffers exist under the path.
- No external process is accessing the path.

This function may prompt some information to the user if needed."
  (interactive "DMount point of the device: ")
  (and (linux-disk--cleanup-buffers-under-root mountpoint)
       (linux-disk--ensure-no-process-running-in-dir mountpoint)))

;;;;; Check if there is no live buffers on a mount point

(defun linux-disk--cleanup-buffers-under-root (mountpoint)
  "Ensure that no Emacs buffer exist under MOUNTPOINT."
  (let ((bufs (linux-disk--buffer-list-under-mountpoint mountpoint)))
    (or (null bufs)
        ;; TODO: Display the buffers
        (when (y-or-n-p (format "Kill %d buffers under the mount point %s? "
                                (length bufs) mountpoint))
          (mapc #'kill-buffer bufs)))))

(defun linux-disk--buffer-list-under-mountpoint (root)
  "Get a list of buffers under ROOT."
  (cl-remove-if-not (lambda (buf) (linux-disk--buffer-under-root-p root buf))
                    (buffer-list)))

(defun linux-disk--buffer-under-root-p (root buf)
  "Test if a buffer (BUF) is associated with a path under ROOT."
  (cl-loop for path in (list (buffer-file-name buf)
                             (with-current-buffer buf default-directory))
           when (and path
                     (string-prefix-p root (file-truename path)))
           return t))

;;;;; Check if there is no process accessing a mount point

(defun linux-disk--ensure-no-process-running-in-dir (mountpoint)
  "Check if any external process is accessing MOUNTPOINT.

This function returns non-nil if there is no such process. If there is a process
accessing the mount point, this function returns nil.

This is done by running either lsof (preferred) or fuser command."
  (let ((procs (cond
                ;; TODO: Kill the processes
                ((executable-find "lsof") (linux-disk--lsof mountpoint))
                ((executable-find "fuser") (linux-disk--fuser mountpoint)))))
    (when procs
      (message "There is a process accessing %s:\n%s" mountpoint procs))
    (null procs)))

(defun linux-disk--lsof (path)
  "Run lsof command on PATH and return its output."
  (with-current-buffer (get-buffer-create linux-disk-lsof-buffer)
    (erase-buffer)
    (when (= 0 (call-process "lsof" nil '(t nil) nil path))
      (buffer-string))))

(defconst linux-disk-lsof-buffer "*lsof*"
  "The name of the buffer to display the output from lsof command.")

(defun linux-disk--fuser (mountpoint)
  "Run fuser command on MOUNTPOINT and return its output."
  (with-current-buffer (get-buffer-create linux-disk-fuser-buffer)
    (erase-buffer)
    (when (= 0 (call-process "fuser" nil '(t nil) nil "-m" mountpoint))
      (buffer-string))))

(defconst linux-disk-fuser-buffer "*fuser*"
  "The name of the buffer to display the output from fuser command.")

(provide 'linux-disk)
;;; linux-disk.el ends here
