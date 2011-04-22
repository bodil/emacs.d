;;; wl-news.el --- Create notification from NEWS(.ja) for Wanderlust.

;; Copyright (C) 2002 Yoichi NAKAYAMA <yoichi@eken.phys.nagoya-u.ac.jp>
;; Copyright (C) 2002 Kenichi OKADA <okada@opaopa.org>

;; Author: Yoichi NAKAYAMA <yoichi@eken.phys.nagoya-u.ac.jp>
;;	Kenichi OKADA <okada@opaopa.org>
;; Keywords: mail, net news

;; This file is part of Wanderlust (Yet Another Message Interface on Emacsen).

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:
;;

;;; Code:
;;

(require 'elmo)
(require 'wl-vars)
(require 'wl-util)
(require 'wl-address)
(require 'wl-folder)

(defvar wl-news-version-file-name "previous-version")
(defvar wl-news-default-previous-version '(2 0 0))

(defvar wl-news-lang
  (if (and (boundp 'current-language-environment)
	   (string-equal "Japanese"
			 (symbol-value 'current-language-environment)))
      '("ja" "en") '("en" "ja"))
	"The list of languages to show NEWS. (order sensitive)")

(defun wl-news-check ()
  (let* ((updated (not (wl-news-already-current-p))))
    (if updated
	(if (and wl-news-lang
		 (wl-news-check-news
		  (cdr (wl-news-previous-version-load))
		  wl-news-lang)
		 (not (memq 'wl-news wl-hook)))
	    (add-hook 'wl-hook 'wl-news))
      ;; update wl-news-version-file
      (wl-news-previous-version-save
       (product-version (product-find 'wl-version))
       (cdr (wl-news-previous-version-load))))
    updated))

;;; -*- news-list -*-

(defconst wl-news-news-alist
  '(("en" ((2 16 0) . "* Changes in 2.16.0 from 2.14.x

** Put spam mark on the message registered as spam.

** Remove spam mark from the message registered as non-spam.

** Added support for ESEARCH feature (RFC4731).

** New option `elmo-imap4-set-seen-flag-explicitly'.

** Optimize refiling.
   Interpret filter, pipe and multi folder and select the most suitable method.

** Speed up bsfilter processing.

** Speed up handling of maildir folder.

** Use IMAP4 non-synchronizing literals (RFC 2088) if it is available.

** Open following thread when you put mark on message in summary buffer.

** Add new command wl-summary-display-raw.

** Use EasyPG (http://www.easypg.org) if it is available.

** A folder type `namazu' is abolished.  New folder type `search' is added instead.

** Fixed against overwriting existing messages in archive folders.
   There was a bug on appending messages.
") ((2 14 1) . "* Changes in 2.14.1 from 2.14.0
  Version 2.14.1 is a bug fix version of 2.14.0.

** Fixed message order of Maildir.

** Icon for the access folder is displayed.
") ((2 14 0) . "* Changes in 2.14.0 from 2.12.2

** New folder type `access' is added.
   In `access' folder, sub-folders of the specified folder can be treated
   as one folder.

** Synchronization speed of the folder is improved.
   The function which calculates list diff is re-wrote and is faster
   than previous implementation, especially in the folders with large
   number of the messages.

** New event handling mechanism is incorporated.

** Improved the disconnected operations for IMAP draft saving.
   There was a bug of message numbering in the disconnected imap draft.
   It is fixed using new event handling mechanism.

** `Shimbun' summary buffers are updated dynamically.
   Some shimbun folder does not have correct information.
   In this version, they are corrected using the message body after retrieval.
   It is implemented with new event handling mechanism.

** Many bug fixes.
") ((2 12 1) . "* Changes in 2.12.1 from 2.12.0
  Version 2.12.1 is a bug fix version of 2.12.0.

** Now Maildir is usable on Windows systems.
   Note that it does not conform to the Maildir standard.

** Fixed the problem of the cache flag inconsistency on the filter folder etc.
   There was a problem that the summary buffer displays cached messages as
   uncached in some folders.

** Fixed the bug that the new flag cannot be changed in some cases.

** Fixed the bug that flag are not taken over correctly from Maildir.
   Only the flag of the first message was taken over in earlier versions.

** Fixed the problem in display module for IMAP messages.
   Now partially fetched messages are displayed correctly.
   If a message included child messages, their headers were not displayed.

** Fixed the problem that %INBOX is not appeared as a subfolder of %INBOX.
   In cyrus-imapd, this problem occurred.

** Now user defined flags are appeared in the completions for search conditions.

** Fixed the problem that a wrong flag folders are created by some flag names.
   If a flag contains a character other than [a-z], the problem occurred.

** Now expansion of the access group \"@/\" works correctly.

** Fixed the problem to cause an error on automatic draft saving.

** Fixed the problem to cause an error on invoking address manager.
   A message which includes a string \"To:\" etc. caused an error.

** Fixed the problem in the flag inheritance function of the filter folder.

** New option `wl-summary-resend-use-cache'.
   You can resend messages using cache in the offline status.

** New option `elmo-network-session-idle-timeout'.
   Network sessions which are not used longer than this value (in seconds)
   are thrown away and new session is created.

** Improved redisplay functions for \"H\" and \"M\" key.
   MIME structure and buffer is reused for redisplay.

** Now attributes for netnews are displayed in the draft preview.
") ((2 12 0) . "* Changes in 2.12.0 from 2.10.1

** The structure of the message database is improved.
   Following setting is to convert legacy msgdb to the new one when you
   select a folder.
   (setq elmo-msgdb-default-type 'standard
         elmo-msgdb-convert-type 'auto)
   (which is initial setting.)

** The temporary mark and corresponding action is now customizable.

   By default, following mark-and-actions are defined.
   mark-and-actions which are defined as before.
    \"o\" refile   (same as before)
    \"O\" copy     (same as before)
    \"d\" dispose  (formerly delete, 'D' mark. Messages are moved to
    		  wl-trash-folder. Its behavior is decided by
		  wl-dispose-folder-alist.)
   New mark-and-actions which are newly introduced.
    \"D\" delete   (remove message immediately)
    \"i\" prefetch (prefetch message)
    \"~\" resend   (resend message)
   Press 'x' to execute actions which corresponds to the mark.
   mark-and-actions can be define by the new variable, 
   'wl-summary-mark-action-list'. See its docstring for more in detail.

** SPAM filter module is added.
   Following spam filter libraries are supported.
   bogofilter
   spamfilter
   bsfilter
   SpamAssassin
   SpamOracle
   Regular Expressions Header Matching

** 'mark folder is renamed to 'flag folder.
   Related to this, original message location is displayed as help-echo on summary
   line in the 'flag folder (you can control the behavior by
   wl-highlight-summary-line-help-echo-alist).

** Now you can put arbitrary user defined flag on message.
   You can specify its flag by \"F\" in the summary mode.

** New marks, 'A' and 'a' are added for answered messages.
   Now answered messages have its own mark in the summary mode.
   'A' is displayed for uncached messages and 'a' is for cached messages.

** New mark,s 'F' and 'f' are added for forwarded messages.
   Now forwarded messages have its own mark in the summary mode.
   'F' is displayed for uncached messages and 'f' is for cached messages.

** New search condition 'Flag' (Status of the message) is added.
   There are flags 'unread', 'important', 'answered',
   'digest' (unread or important) and 'any' (any of the flag).
   For example, following filter folder contains only unread or important
   messages in the %inbox folder.

   /flag:digest/%inbox

** Draft save function is improved.
   Now you can set wl-draft-folder to IMAP folder, Maildir folder, and so on.

** Automatically save draft buffers by using idle-timer.
   You can control behavior by the variable `wl-auto-save-drafts-interval'.

** 'H' key(display all header) and 'M' key(display without MIME analysis)
   are now toggle key.
   Now you can cite messages displayed by 'M'.

** Now you can sort summary lines into descending order.

** Abbreviate too long header extended to lines in message buffer.

** Persistent mark string in summary buffer is changed.
   Default setting indicates cached state by its upper/lower case.

** It displays draft preview on sending confirmation.

** Sending parameters are displayed on draft preview.
   See description of the variable wl-draft-preview-attribute for detail.

** You can run biff with idle-timer by setting wl-biff-use-idle-timer.

** Now wl-draft-kill confirms with yes-or-no-p.

** Summary thread will be divided if its depth is larger than certain amount.
   The limit is controlled by the variable wl-summary-max-thread-depth.

** Emacs multi-tty support is supported.
   (http://lorentey.hu/project/emacs.html)

** New sort spec 'size' is added in the summary mode.
   Now you can sort the summary by message size.

** The variable wl-refile-policy-alist is abolished.

** Batch processing module is added.

** In the multi-folder, status of messages are synchronized with original
   folder.
   For example, unread status of '+inbox' is updated to '*+inbox,+outbox'.

** The function wl-summary-resend-message is abolished.
   you can put mark for resending by wl-summary-resend instead.

** Variables renamed
   wl-delete-folder-alist is renamed to wl-dispose-folder-alist.

** POP3 folder existence check is simplified (by default).
   The default value for elmo-pop3-exists-exactly is changed to nil.

** POP3 response code extension defined in the RFC2449 is supported.
   If a login failure occurred because of user's another POP3 session, 
   entered password is not cleared and used in the future login.

** IMAP4 commands EXPUNGE and CHECK are now send asynchronously.

** Default value of wl-folder-hierarchy-access-folders has been changed.

** Access group \"@/\" of shimbun folders can be used now.

** Show contents of NEWS(.ja) when you start Wanderlust newer than the
   one you used previously.

** Default values of wl-draft-reply-*-list are changed. 
   See samples/en/dot.wl for old values.

** wl-draft-reply-myself-*-list are abolished and integrated into
   wl-draft-reply-*-list.

** You can control initial cursor position for replying draft.
   Set variable wl-draft-reply-default-position appropriately.

** Changed the way to specify configuration of draft buffer window.
   You can choose keep, full or split as values of wl-draft-buffer-style
   and wl-draft-reply-buffer-style.

** Commands to verify/decrypt non-MIME PGP message are added.
   C-c:v, C-c:d in message buffer to verify or decrypt respectively.

** New hooks
   wl-draft-reply-hook
   wl-summary-reply-hook
   wl-draft-forward-hook
   wl-summary-forward-hook
   wl-draft-kill-pre-hook
   wl-summary-resend-hook

** Abolished hook
   wl-reply-hook

** New face

   wl-highlight-summary-disposed-face
   wl-highlight-summary-prefetch-face
   wl-highlight-summary-resend-face
   wl-highlight-summary-answered-face
   wl-highlight-action-argument-face

** Abolished face

   wl-highlight-refile-destination-face
   (renamed to wl-highlight-action-argument-face)
") ((2 10 1) . "* Changes in 2.10.1 from 2.10.0
  Version 2.10.1 is a bug fix version of 2.10.0.

** Fixed the problem that msgdb be destroyed when print-length or
   print-level has Non-nil value.

** wl-summary-pack-number in pipe folder is disabled temporarily
   since it didn't work. Invoke it in destination folder instead.

** Fixed a problem that wl-folder-move-cur-folder doesn't work.

** Fixed a problem that wl-draft-reedit doesn't work properly on Meadow.

** Fixed a problem that wl-summary-pack-number doesn't work on Maildir and
   shimbun folders.

** Fixed a problem that cache file is not protected even if it is marked
   as important.

** Fixed a problem that %# in wl-summary-line-format cannot handle large
   number.

** Fixed a problem to remove password even if SMTP AUTH failed at non-auth
   phase.

** Default value of wl-message-buffer-prefetch-folder-type-list,
   wl-message-buffer-prefetch-idle-time, and
   wl-message-buffer-prefetch-depth are changed.

** Fixed to compile on XEmacs without mule feature.
") ((2 10 0) . "* Changes in 2.10.0 from 2.8.1

** You can alter the format of summary lines.
   Specify format by wl-summary-line-format. If you want to change ones
   according to folder names, use wl-folder-summary-line-format-alist.

** Save format for the draft folder has been changed. Messages are encoded
   before saved by wl-draft-save.

** elmo-split is newly established. It provides a way to split messages
   according to some rule a la procmail.

** Buffer prefetch works fine now. Messages of the number specified by
   wl-message-buffer-prefetch-depth are loaded into buffer in advance.

** elmo-dop-queue-flush flushes queue that concerns plugged folder.

** Starting Wanderlust on the new frame is possible now. Set as
   (autoload 'wl-other-frame \"wl\" \"Wanderlust on new frame.\" t)

** In Folder mode, you can go into virtual folder which consists of messages
   with some specified condition (wl-folder-virtual). It is binded to \"V\".

** In Folder mode, you can search folders containing messages with some
   specified condition (wl-folder-pick). It is binded to \"?\".

** Now you can rename access group folders.

** You can specify ON/OFF of thread view for newly created summary.
   Set wl-summary-default-view, wl-summary-default-view-alist.

** Temporary marks are kept when you exit from sticky summary by q or g. 

** Key bindings concerning the sticky summary have been changed.
   By C-u g, the sticky summary is destroyed as well as C-u q. In summary or
   folder mode, G opens the sticky summary.

** You can go round summary buffers by C-cC-n and C-cC-p.

** Members of the list wl-folder-hierarchy-access-folders is now some REGEXP
   for access group names instead of exact group names.

** In header part of the draft buffer C-a brings cursor to the beginning of
   the line or the beginning of the header body.

** You can send encapsulated blind carbon copies. Its default field name is
   \"Ecc:\".

** C-c C-y (Draft) can cite region of the message.
   It affects if transient-mark-mode (Emacs) or zmacs-regions (XEmacs) is
   Non-nil and the region is active.

** You can delete a part from multipart message.
   It is binded as \"D\" in message buffer.

** You can easily configure server settings to post news article.
   Set wl-nntp-posting-config-alist appropriately. See Info for an example.

** You can specify some function in wl-draft-reply-with-argument-list etc.
   for setting the recipients in draft by the return value of it.

** The interface of the function wl-draft has been changed.
   The initial set of headers are handed as an association list.

** The uses of wl-generate-mailer-string-function has been changed.
   Specify a function which returns some string to appear in User-Agent header.

** The Reference Card (doc/wl-refcard.tex) describes important key bindings.

** Many bug fixes.
") ((2 8 0) . "* Changes in 2.8.0 from 2.6.1

** Nemacs, Mule 2.3 based on Emacs 19.28 are not supported any longer.

** Wanderlust might not work with FLIM 1.14.2 and older.
   It is recommended to use FLIM 1.14.3 or newer and associated SEMI.

** Now available `make check' environment test for user.

** If you set obsolete variables (e.g. renamed ones) in .wl etc, Wanderlust
   shows warning messages and urge you to change settings.
   Change your settings according to the messages, please.
   If you want to suppress warnings, set elmo-obsolete-variable-show-warnings
   to nil.

** Added new internal folders: 'sendlog folder

** Added new type of folders: shimbun folder

   Format: '@' 'virtual server name' '.' 'group name'

** Added new type of folders: namazu folder

   Format:  '[' 'search condition' ']' [ 'absolute path of namazu index' ]

** With pipe folder, now you can preserve messages on the server.
   At the next time you access it, only new messages will be copied.

   Format:  '|' 'source folder' '|:' 'destination folder'

** Address manager is now available (start by C-c C-a).
   You can edit address book and import recipients to draft from it.

** ACAP (RFC2244) is supported(experimental).

** Now you can preserve IMAP4 message by part as a cache.
   If you skipped enormous part, you can read other than skipped part when
   you are off line.

** Wanderlust also creates message view through prefetching.
   Displaying of prefetched messages speeded up because of this.

** Truncation of lines in message buffer or draft buffer is now controllable.
   Non-nil value of wl-message-truncate-lines or wl-draft-truncate-lines
   means truncating long lines at window width.

** Bitmap image for opening demo is removed from wl-demo.elc and now loaded
   from wl-icon-directory.
   Special logo is displayed through the Christmas season :)

** Overall elmo module is rewritten.

** Variables depending on elmo backends are renamed to \"elmo-backend-*\".
   e.g. elmo-default-imap4-server is renamed to elmo-imap4-default-server.

** Variables named xxx-func are renamed to xxx-function.

** X-Face utility 1.3.6.12 or older is not supported any longer.
   Please install X-Face utility 1.3.6.13 or later, if necessary.

** Wanderlust distinguishes stream-type on plugged mode. They are treated as
   different entries.

** msgdb path for archive and multi folders are changed.
   No problem for running wanderlust even if you do not deal with them.
   But if you don't want to leave useless data on the disk, delete under
   .elmo/multi and .elmo/archive in advance.

** Variables named xxx-dir are renamed to xxx-directory.
   e.g. wl-icon-dir is renamed to wl-icon-directory.
   Take attention if you set for display of startup logo, etc.

** elmo-cache-dirname is abolished and elmo-cache-directory is newly created.
   You can put cache directory to another place by setting
   elmo-cache-directory.

** Default value of elmo-enable-disconnected-operation is now `t'.
   When the relevant messages are cached, you can do some operations
   even in the off-line state.

** Now messages with \"$\" mark is not remained in the summary buffer when
   the actual message itself is deleted.
   Please visit the 'mark folder to review the messages with the \"$\" mark.
") ((2 6 1) . "* Changes in 2.6.1 from 2.6.0
  Version 2.6.1 is basically a bug fix version of 2.6.0.

** Fixed a problem that Emacs 21 causes `Recursive load...' error.

** Fixed a problem that thread character is broken in XEmacs 21.1.

** Fixed a problem that in IMAP4 folder, progress bar is remained in XEmacs .

** Fixed a problem that searching is failed for the header fields that
   begins with X-.

** Some other fixes.
") ((2 6 0) . "* Changes in 2.6.0 from 2.4.1

** FLIM 1.13.x is not supported any longer.
   Please install FLIM 1.14.1 or later.

** Now folder and summary buffer can be opened in a separate frame.
   If `wl-folder-use-frame' is set as t, `M-x wl' creates a new frame
   for folder mode. If `wl-summary-use-frame' is set as t, new frames
   are created for each summary window.

** Cursor moving speed ('N' or 'P' in summary) is greatly improved.

** Folder checking speed for filter folder of localdir
   folder using `last' or `first' (Ex. /last:100/+inbox) is improved.

** Retrieval progress of each message is displayed in POP and IMAP folder.

** Coloring of summary buffer is processed on demand (only on Emacs).
   If `wl-summary-lazy-highlight' is non-nil, 
   only visible portion of the buffer is colored.

** Customizable biff notify.
   New hook `wl-biff-notify-hook' and `wl-biff-unnotify-hook' is
   now available.
   e.g. (add-hook wl-biff-notify-hook 'ding)

** Many bug fixes.
") ((2 4 1) . "* Changes in 2.4.1 from 2.4.0
  Version 2.4.1 is basically a bug fix version of 2.4.0.

** Wanderlust 2.4.1 now works on FLIM 1.14.x. 

** Fixed a problem that POP connection remains after POP before SMTP.

** The specification of IMAP4 authentication method for clear password
   is changed.

In 2.4.0, To use clear password authentication method in IMAP4
\(Logging in with LOGIN command), you have to set the variable
`elmo-default-imap4-authenticate-type' as 'plain (or nil).
But in 2.4.1, it is changed to 'clear (or nil).
Example:
\(setq elmo-default-imap4-authenticate-type 'plain)
should be changed to
\(setq elmo-default-imap4-authenticate-type 'clear)
") ((2 4 0) . "* Changes in 2.4.0 from 1.1.1

** Version Number
The version numbering convention for Wanderlust is changed. 

In earlier versions, 1.x were stable version and from 2.0.x to 2.2.x
were beta version. But since version 2.3.0, the second (minor) version
number implies the stability of the Wanderlust. Even minor number
corresponds to a stable version, and an odd minor number corresponds
to a development version. This version numbering is based on the
widespread convention of open source development.

On the open CVS server cvs.m17n.org, main trunk contains the current
beta (newest experimental) version, and branches contain the stable
version.  (If the version is 2.4.x, the branch name is wl-2_4)

** Install

*** FLIM 1.12 is not supported anymore.
See the file INSTALL for details.

*** APEL 10.2 or later is required.
tm-8 users should check the version of APEL (tm-8.8 contains old APEL).

** New feature

*** LDAP support
Complete e-mail address in draft by searching LDAP server.
If the variable wl-use-ldap is non-nil, LDAP feature is enabled
\(Initial setting is nil).

*** UIDL support in POP3 folder
POP3 folder now saves the status of summary and it improves summary
update speed. If the variable elmo-pop3-use-uidl is non-nil, UIDL is
used (Initial setting is t).

*** Emacs 21 support
Wanderlust has started on supporting Standard Emacs 21.
Toolbars and icon images can be shown in almost Wanderlust
frames like XEmacs.

*** biff feature
Server mailbox is checked periodically.
If new mail is arrived, Wanderlust changes the biff (icon) on the modeline
and updates folder mode content.

*** expire-hide 
Now expire mechanism has new feature `hide', it does not remove
messages actually from folder but hides messages from summary. It
improves processing speed for large folders.

*** Message thread restoring feature
Automatic correction of broken threads by subject matching is now available.
Thread modification by hand (M-w (copy) and C-y (paste) in summary mode)
is also available.

*** Password expiration timer
Password cache expires after elmo-passwd-life-time is passed.
\(nil means no expiration. Initial setting is nil)

*** killed-list
Deleted messages in the NNTP folder are saved to `killed-list'.  The
messages in the killed-list are treated as if it were not exist on the
server. Non-nil value for elmo-use-killed-list enables this feature
\(Initial setting is t). By this feature, NNTP pipe folder works correctly.

*** Maildir pack is now available
M-x wl-summary-pack-number in the summary mode of Maildir folder
re-numbers the messages.

** Searching

*** Complex condition can be specified for filter folder
AND condition, OR condition, NOT condition, and their combination can be
 specified. Syntax of the condition part is changed. See Info for details.

Caution for those who upgrade from 1.1.1:
By this change, saving directory for the msgdb of filter folder is altered.
Former msgdbs are not needed anymore. It does not cause any problem but
if you don't want to keep useless disk, you should remove files
under the directory '.elmo/filter/' beforehand.

*** Searching of the NNTP folder is available
Now you can make NNTP filter folder.
\(If only your NNTP server responds to XHDR command.)

*** Pick, Virtual in summary mode now accepts complex condition.
You can set AND condition and OR condition by typing
'AND' or 'OR' instead of field name.

** Session, Authentication

*** elmo-default-*-authenticate-type only accepts symbol(used be a string)
Example:
\(setq elmo-default-imap4-authenticate-type \"cram-md5\")
should be changed to
\(setq elmo-default-imap4-authenticate-type 'cram-md5)

*** stream-type can be defined.
You can define stream type by
elmo-network-{imap4-,pop3-,nntp-,}stream-type-alist.
Some SSL related variables are abolished(renamed).
You can access to the networked folders (IMAP4, NNTP, POP3) via SOCKS
if you specify the folder name end with \"!socks\".

** Draft

*** group-list is now available
You can specify address like 'Group: foo@gohome.org, bar@gohome.org;'.
If wl-draft-remove-group-list-contents is non-nil, the contents of 
group-list is removed before sending.

*** The draft preview displays recipient addresses on minibuffer 
You can confirm the group-list recipients by this.

*** Initial setting considers Reply-To:.
Default setting of wl-draft-reply-without-argument-list considers Reply-To: 
field (Set to To: field).

*** Replying rules for the messages sent from yourself.
You can define replying rules for the messages sent from yourself by
setting wl-draft-reply-myself-with-argument-list and
wl-draft-reply-myself-without-argument-list.

*** Full name is used in the reply address.
If wl-draft-reply-use-address-with-full-name is non-nil, then full
name is inserted in with e-mail addresses on the replied message
\(Initial setting is t).

*** In-Reply-To: format is changed.
In-Reply-To: format is changed to simple one. It is based on 
draft-ietf-drums-msg-fmt-09.txt.

** misc

*** Message thread processing is improved.

*** Renamed variables
wl-refile-guess-func-list => wl-refile-guess-functions
wl-summary-temp-above => wl-summary-target-above

*** You can set function to wl-fcc.
You can change fcc folder name dynamically. For example, change folder name
by month.

*** elmo-search-mime-charset is abolished.
Charset is guessed from the string you typed.

*** Useless headers are removed when you forward the message.
You can specify removed headers by wl-ignored-forwarded-headers.

*** wl-highlight-group-folder-by-numbers is abolished.
It is renamed to wl-highlight-folder-by-numbers and has following meaning.
  `t'   : Whole line is colored by message number.
  `nil' : Whole line is colored by folder status.
   Number (ex. `1') : Line is colored by message number and folder status.

*** Header visibility control is changed.
Header visibility is controlled by Wanderlust (was controlled by SEMI).
You can change header visibility by wl-message-ignored-field-list and 
wl-message-visible-field-list.

*** DEMO is changed.
Less colors are used by DEMO pixmap.
Emacsen on character based terminal also display suitable DEMO.
") ((1 1 1) . "* Changes in 1.1.1 from 1.1.0
  Version 1.1.1 is a bug fix version of 1.1.0 with minor user-visible changes.

** Development on the CVS server is started.

** Flush operation and sending queues if Wanderlust is  started
   in plugged status.

** Directory structure is changed.

*** 00README, 00README.ja is renamed to README, README.ja.

*** All wl-* files are moved to the directory 'wl'.

** Syntax of wl-refile-rule-alist is extended (compatible with older one).

** progress gauge
Progress gauge is displayed while processing in the Emacsen with
progress gauge feature.
") ((1 1 0) . "* Changes in 1.1.0 from 1.0.3

** Install

*** tm7 is not supported anymore.
see the file INSTALL for details.

*** WL_PREFIX and ELMO_PREFIX default as \"wl\"
\(defvar WL_PREFIX \"wl\")
\(defvar ELMO_PREFIX \"wl\")

e.g. install directory is
  1.0.3  /usr/local/share/emacs/site-lisp/
  1.1.0  /usr/local/share/emacs/site-lisp/wl/

*** Change default macro in Makefile.
EMACS   = emacs
XEMACS  = xemacs
use $(XEMACS), `package' and `install-package' target.

*** Install not only *.elc, but also *.el.

*** English document (wl.texi).

** New feature

*** Modified UTF7 support.
Now international mailbox name can be used in IMAP4 in the Emacsen
with unicode feature.

*** Scoring support.

*** New plugged system.

*** IMAP4 support became more generic.
Many IMAP4 servers are supported.

*** New authentication type
  IMAP4: CRAM-MD5, DIGEST-MD5, STARTTLS
  POP3:  CRAM-MD5, DIGEST-MD5, SCRAM-MD5, STARTTLS
  NNTP:  STARTTLS
  SMTP:  STARTTLS

*** New folder type
  |      Pipe Folder     Incorporate message.
  .      Maildir Folder  Now Maildir is one of the folder type.
  'cache Cache Folder    View internal cache.

*** Message buffer cache
Next message is prefetched while idle time.

*** Sticky summary is enhanced.
Now message buffer is also sticky.
You can specify always-sticky summary.

** misc

*** Eliminated wl-draft-prepared-config-alist
unified with wl-draft-config-alist.

*** POP-before-SMTP variables are re-arranged.

*** Ask non-existing folder.
 When FCC: contains new folder.
 When auto-refile specified new folder.

*** Change fetch threshold and confirm settings.
wl-prefetch-confirm-threshold, wl-cache-fetch-threshold.

*** Can use petname for completion.

*** Change Message-ID generator.

*** wl-demo.el support bitmap-mule.

*** Allow function type `smtp-server' value.

*** Make sendlog when `wl-draft-sendlog' is non-nil.

*** `wl-summary-incorporate-marks'

*** Reserve prefetching while off-line status.

*** Draft use new frame when `wl-draft-use-frame' is non-nil.

*** New variable `wl-user-mail-address-list' .

*** New variable `wl-local-domain' for set FQDN.

*** Server side unread status is used in IMAP4 folder.

*** Change defaults
  wl-mime-charset         iso-2022-jp  =>  x-ctext
  wl-summary-move-order   'new  =>  'unread
  wl-tmp-dir              TMPDIR  =>  ~/tmp/

*** New hooks
  wl-draft-send-hook
  wl-draft-reedit-hook
  wl-mime-edit-preview-message-hook
  wl-folder-suspend-hook
  wl-summary-toggle-disp-folder-message-resumed-hook
  wl-summary-line-inserted-hook
  wl-thread-update-children-number-hook
  mmelmo-header-inserted-hook
  mmelmo-entity-content-inserted-hook

*** New function
  wl-save
  wl-summary-write
  wl-summary-supersedes-message
  wl-fldmgr-delete
  wl-refile-guess-by-msgid
  wl-address-user-mail-address-p
  wl-summary-jump-to-msg-by-message-id-via-nntp
  wl-summary-temp-mark-pick
"))))

;;; -*- news-list-end -*-

(defun wl-news-previous-version-load ()
  (with-temp-buffer
    (let ((filename (expand-file-name
		     wl-news-version-file-name
		     elmo-msgdb-directory))
	  insert-file-contents-pre-hook
	  insert-file-contents-post-hook
	  ret-val)
      (if (not (file-readable-p filename))
	  (cons wl-news-default-previous-version
		wl-news-default-previous-version)
	(insert-file-contents filename)
	(condition-case nil
	    (read (current-buffer))
	  (error nil nil))))))

(defun wl-news-previous-version-save (current-version previous-version)
  (with-temp-buffer
    (let ((filename (expand-file-name
		     wl-news-version-file-name
		     elmo-msgdb-directory))
	  print-length print-level)
      (prin1 (cons current-version previous-version) (current-buffer))
      (princ "\n" (current-buffer))
      (if (file-writable-p filename)
	  (write-region (point-min) (point-max)
			filename nil 'no-msg)
	(message "%s is not writable." filename)))))

(defun wl-news-append-news (lang previous-version &optional no-mime-tag)
  (require 'wl-mime)
  (let ((news-list (cdr (assoc lang wl-news-news-alist)))
	ret)
    (when news-list
      (if no-mime-tag
	  (insert "--------------\n")
	(mime-edit-insert-tag "text" "plain" "" ""))
      (while (< 0
		(product-version-compare
		 (car (car news-list))
		 previous-version))
	(setq ret t)
	(insert (cdr (car news-list)) "\n\n")
	(setq news-list (cdr news-list))))
    ret))

(defun wl-news-check-news (version news-lang)
  (let ((lang news-lang)
	news-list ret)
    (while (car lang)
      (setq news-list (cdr (assoc (car lang) wl-news-news-alist)))
      (while (< 0
		(product-version-compare
		 (car (car news-list)) version))
	(setq ret t)
	(setq news-list (cdr news-list)))
      (setq lang (cdr lang)))
    ret))

(defun wl-news-already-current-p ()
  (>= 0 (product-version-compare
	 (product-version (product-find 'wl-version))
	 (car (wl-news-previous-version-load)))))

(defun wl-news-send-news (version news-lang folder)
  (require 'wl-draft)
  (let ((lang (if (listp wl-news-lang)
		  wl-news-lang
		(list wl-news-lang)))
	send-buffer
	wl-fcc wl-bcc ret)
    (save-window-excursion
      (set-buffer
       (setq send-buffer (wl-draft-create-buffer)))
      (wl-draft-create-contents
       (list (cons 'From "WL Release 'Bot <wl@lists.airs.net>")
	     (cons 'To (wl-draft-eword-encode-address-list wl-from))
	     (cons 'Subject "Wanderlust NEWS")
	     (cons 'Date (wl-make-date-string))
	     (cons 'User-Agent wl-generate-mailer-string-function)))
      (wl-draft-insert-mail-header-separator)
      (wl-draft-prepare-edit)
      (goto-char (point-max))
      (insert "\nThis message is automatically generated by Wanderlust.\n\n")
      ;; insert news
      (while (car lang)
	(wl-news-append-news (car lang) version)
	(setq lang (cdr lang)))
      ;; encode
      (let ((mime-header-encode-method-alist
	     '((eword-encode-unstructured-field-body))))
	(mime-edit-translate-buffer))
      (wl-draft-get-header-delimiter t)
      (setq ret
	    (and (elmo-folder-writable-p
		  (wl-folder-get-elmo-folder folder))
		 (elmo-folder-append-buffer
		  (wl-folder-get-elmo-folder folder))))
      (wl-draft-hide send-buffer)
      (wl-draft-delete send-buffer))
    ret))

;;; wl-news-mode

(defvar wl-news-buf-name "NEWS")
(defvar wl-news-mode-map nil)
(defvar wl-news-winconf nil)
(defvar wl-news-buffer-oldest-version nil)
(make-variable-buffer-local 'wl-news-buffer-oldest-version)

(unless wl-news-mode-map
  (setq wl-news-mode-map (make-sparse-keymap))
  (define-key wl-news-mode-map "q"     'wl-news-exit)
  (define-key wl-news-mode-map "Q"     'wl-news-force-exit)
  (define-key wl-news-mode-map "\C-xk" 'wl-news-exit)
  (define-key wl-news-mode-map "a"     'wl-news-show-all)
  (define-key wl-news-mode-map "m"     'wl-news-append-to-folder)
  (define-key wl-news-mode-map "\C-m"  'wl-news-next-line)
  (define-key wl-news-mode-map " "     'wl-news-next-page)
  (define-key wl-news-mode-map "\177"  'wl-news-previous-page)
  ;; re-bind commands of outline-mode
  (define-key wl-news-mode-map "n"     'outline-next-visible-heading)
  (define-key wl-news-mode-map "p"     'outline-previous-visible-heading)
  (define-key wl-news-mode-map "u"     'outline-up-heading)
  (define-key wl-news-mode-map "N"     'outline-forward-same-level)
  (define-key wl-news-mode-map "P"     'outline-backward-same-level))

(require 'derived)
(define-derived-mode wl-news-mode outline-mode "NEWS"
  "Mode for Wanderlust NEWS(.ja)."
  (setq buffer-read-only t))

(defun wl-news (&optional arg)
  (interactive "P")
  (remove-hook 'wl-hook 'wl-news)
  (let* ((previous-version (if arg wl-news-default-previous-version
			     (cdr (wl-news-previous-version-load))))
	 (lang wl-news-lang)
	 window-lines lines)
    (if (or (get-buffer wl-news-buf-name)
	    (if (wl-news-check-news previous-version wl-news-lang)
		(progn
		  (setq wl-news-winconf (current-window-configuration))
		  (set-buffer (get-buffer-create wl-news-buf-name))
		  (wl-news-mode)
		  (setq wl-news-buffer-oldest-version previous-version)
		  (buffer-disable-undo (current-buffer))
		  ;; insert news
		  (let ((buffer-read-only nil))
		    (insert "--- Wanderlust NEWS ---  press 'a' to show all NEWS\n")
		    (insert "                         press 'm' to mail this NEWS to your folder\n")
		    (insert "                         press 'q' to quit\n")
		    (insert "                         press 'Q' to force quit\n\n")
		    (while (car lang)
		      (wl-news-append-news
		       (car lang) previous-version t)
		      (setq lang (cdr lang))))
		  t)
	      (message "No NEWS.")
	      nil))
	(progn
	  (switch-to-buffer wl-news-buf-name)
	  (delete-other-windows)
	  (goto-char (point-min))))))

(defun wl-news-next-line ()
  (interactive)
  (scroll-up 1))

(defun wl-news-next-page ()
  (interactive)
  (scroll-up))

(defun wl-news-previous-page ()
  (interactive)
  (scroll-down))

(defun wl-news-show-all ()
  (interactive)
  (when (eq major-mode 'wl-news-mode)
    (kill-buffer (current-buffer))
    (wl-news t)))

(defun wl-news-exit ()
  (interactive)
  (let* ((oldest-version (cdr (wl-news-previous-version-load)))
	 (current-version (product-version (product-find 'wl-version)))
	 (new-old-version current-version)
	 (buf (get-buffer wl-news-buf-name)))
    (when buf
      (if (wl-news-check-news oldest-version wl-news-lang)
	  (if (y-or-n-p "Do you want to see this message again? ")
	      (progn
		(message "Please M-x wl-news if you want to see it.")
		(setq new-old-version oldest-version))))
      (wl-news-previous-version-save
       current-version new-old-version)
      (kill-buffer (current-buffer))
      (if wl-news-winconf
	  (set-window-configuration wl-news-winconf))
      (kill-buffer buf)
      (if wl-news-winconf
	  (set-window-configuration wl-news-winconf)))))

(defun wl-news-append-to-folder ()
  (interactive)
  (let* ((current-version (product-version (product-find 'wl-version)))
	 (new-old-version current-version)
	 (folder wl-default-folder))
    (if (or (and (elmo-folder-writable-p
		  (wl-folder-get-elmo-folder folder))
		 (y-or-n-p (format
			    "Do you want to append this message to %s ? "
			    wl-default-folder)))
	    (setq folder
		  (wl-summary-read-folder wl-default-folder "to append ")))
	(or (wl-news-send-news wl-news-buffer-oldest-version wl-news-lang folder)
	    (error "Cannot append NEWS mail to %s" folder)))))

(defun wl-news-force-exit ()
  (interactive)
  (let ((buf))
    (when (setq buf (get-buffer wl-news-buf-name))
      (wl-news-previous-version-save
       (product-version (product-find 'wl-version))
       (cdr (wl-news-previous-version-load)))
      (kill-buffer buf)
      (if wl-news-winconf
	  (set-window-configuration wl-news-winconf)))))


(require 'product)
(product-provide (provide 'wl-news) (require 'wl-version))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; wl-news.el ends here
