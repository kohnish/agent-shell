;;; agent-shell-devin.el --- Devin agent configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file includes Devin specific configurations relying on the
;; devin: https://docs.factory.ai/cli/getting-started/quickstart
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'shell-maker)
(require 'acp)

(declare-function agent-shell--indent-string "agent-shell")
(declare-function agent-shell-make-agent-config "agent-shell")
(autoload 'agent-shell-make-agent-config "agent-shell")
(declare-function agent-shell--make-acp-client "agent-shell")
(declare-function agent-shell--dwim "agent-shell")

(cl-defun agent-shell-devin-make-authentication (&key api-key none)
  "Create Devin authentication configuration.

API-KEY is the Devin API key string or function that returns it.
NONE when non-nil disables API key authentication (e.g., when devin-acp
is already logged in or uses an alternative auth flow).

Only one of API-KEY or NONE should be provided, never both."
  (when (and api-key none)
    (error "Cannot specify both :api-key and :none - choose one"))
  (unless (or api-key none)
    (error "Must specify either :api-key or :none"))
  (cond
   (api-key `((:api-key . ,api-key)))
   (none `((:none . t)))))

(defcustom agent-shell-devin-authentication
  (agent-shell-devin-make-authentication :none t)
  "Configuration for Devin authentication.
For API key (string):

  (setq agent-shell-devin-authentication
        (agent-shell-devin-make-authentication :api-key \"your-key\"))

For API key (function):

  (setq agent-shell-devin-authentication
        (agent-shell-devin-make-authentication :api-key (lambda () ...)))

For no authentication (e.g., using `devin-acp` built-in login):

  (setq agent-shell-devin-authentication
        (agent-shell-devin-make-authentication :none t))"
  :type 'alist
  :group 'agent-shell)

(defcustom agent-shell-devin-default-model-id
  nil
  "Default Devin AI model ID.

Must be one of the model ID's displayed under \"Available models\"
when starting a new shell."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defcustom agent-shell-devin-default-session-mode-id
  nil
  "Default Devin AI session mode ID.

Must be one of the mode ID's displayed under \"Available modes\"
when starting a new shell."
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defcustom agent-shell-devin-default-reasoning-effort
  nil
  "Default reasoning effort.

It can be one of the followings.
none, dynamic, off, minimal, low, medium, high, xhigh, max"
  :type '(choice (const nil) string)
  :group 'agent-shell)

(defcustom agent-shell-devin-acp-command
  '("devin" "acp")
  "Command and parameters for the Devin ACP client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-devin-environment
  nil
  "Environment variables for the Devin ACP client.

This should be a list of environment variables to be used when
starting the Devin client process.

Example usage to set custom environment variables:

  (setq agent-shell-devin-environment
        (`agent-shell-make-environment-variables'
         \"MY_VAR\" \"some-value\"
         \"MY_OTHER_VAR\" \"another-value\"))"
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-devin-make-agent-config ()
  "Create a Devin agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'devin
   :mode-line-name "Devin"
   :buffer-name "Devin"
   :shell-prompt "Devin> "
   :shell-prompt-regexp "Devin> "
   :icon-name "https://avatars.githubusercontent.com/u/131064358"
   :welcome-function #'agent-shell-devin--welcome-message
   :client-maker (lambda (buffer) (agent-shell-devin-make-client :buffer buffer))
   :default-model-id (lambda () agent-shell-devin-default-model-id)
   :default-session-mode-id (lambda () agent-shell-devin-default-session-mode-id)
   :install-instructions "See https://https://docs.devin.ai/get-started/devin-intro for installation."))

;;;###autoload
(defun agent-shell-devin-start-agent ()
  "Start an interactive Devin agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-devin-make-agent-config)
                     :new-shell t))

(cl-defun agent-shell-devin-make-client (&key buffer)
  "Create a Devin client using BUFFER as context.

Uses `agent-shell-devin-authentication' for authentication configuration."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (when (and (boundp 'agent-shell-devin-command) agent-shell-devin-command)
    (user-error "Please migrate to use agent-shell-devin-acp-command and eval (setq agent-shell-devin-command nil)"))
  (let ((api-key (agent-shell-devin-key)))
    (agent-shell--make-acp-client :command (car agent-shell-devin-acp-command)
                                  :command-params (append (seq-rest agent-shell-devin-acp-command))
                                  :environment-variables (append (cond ((map-elt agent-shell-devin-authentication :none)
                                                                        nil)
                                                                       (api-key
                                                                        (list (format "DEVIN_API_KEY=%s" api-key)))
                                                                       (t
                                                                        (error "Missing Devin authentication (see agent-shell-devin-authentication)")))
                                                                 agent-shell-devin-environment)
                                  :context-buffer buffer)))

(defun agent-shell-devin-key ()
  "Get the Devin API key."
  (cond ((stringp (map-elt agent-shell-devin-authentication :api-key))
         (map-elt agent-shell-devin-authentication :api-key))
        ((functionp (map-elt agent-shell-devin-authentication :api-key))
         (condition-case _err
             (funcall (map-elt agent-shell-devin-authentication :api-key))
           (error
            (error "API key not found.  Check out `agent-shell-devin-authentication'"))))
        (t
         nil)))

(defun agent-shell-devin--welcome-message (config)
  "Return Devin welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-devin--ascii-art)))
        (message (string-trim-left (shell-maker-welcome-message config) "\n")))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-devin--ascii-art ()
  "Devin ASCII art."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (text (string-trim "
██████╗ ███████╗██╗   ██╗██╗███╗   ██╗
██╔══██╗██╔════╝██║   ██║██║████╗  ██║
██║  ██║█████╗  ██║   ██║██║██╔██╗ ██║
██║  ██║██╔══╝  ╚██╗ ██╔╝██║██║╚██╗██║
██████╔╝███████╗ ╚████╔╝ ██║██║ ╚████║
╚═════╝ ╚══════╝  ╚═══╝  ╚═╝╚═╝  ╚═══╝
" "\n")))
    (propertize text 'font-lock-face (if is-dark
                                         '(:foreground "#8b949e" :inherit fixed-pitch)
                                       '(:foreground "#444" :inherit fixed-pitch)))))

(provide 'agent-shell-devin)

;;; agent-shell-devin.el ends here
