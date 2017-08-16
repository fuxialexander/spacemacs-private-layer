;;; org-ref-ivy.el --- org-ref with ivy completion -*- lexical-binding: t; -*-

;; Copyright (C) 2016  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/org-ref
;; Version: 1.0.0
;; Keywords: org-mode, cite, ref, label
;; Package-Requires: ((dash "2.11.0") (ivy "0.9.0") (hydra "0.13.2") (s "1.10.0") (f "0.18.0") (parsebib "0")  (emacs "24.4"))

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Lisp code to setup bibliography, cite, ref and label org-mode links.
;; Also sets up reftex and ivy for org-mode citations.  The links are
;; clickable and do things that are useful.

;; You should really read org-ref.org in this package for details.

;;; Code:
(require 'org-ref-core)
(require org-ref-completion-library)

(provide 'org-ref-ivy)

;;; org-ref-ivy.el ends here
