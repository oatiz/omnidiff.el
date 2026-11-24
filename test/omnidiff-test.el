;;; omnidiff-test.el --- ERT test entrypoint for omnidiff -*- lexical-binding: t; -*-

;; Copyright (C) 2026 oatiz

;; This file is part of omnidiff.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Top-level ERT test suite entrypoint for `omnidiff.el'.  Loads all modular
;; `test/omnidiff-*-test.el' suites in dependency DAG order so running
;; `make test' or `make test-src' executes the entire suite in one process.

;;; Code:

(require 'omnidiff-theme-test)
(require 'omnidiff-engine-test)
(require 'omnidiff-render-test)
(require 'omnidiff-dual-test)
(require 'omnidiff-image-test)
(require 'omnidiff-git-test)

(provide 'omnidiff-test)
;;; omnidiff-test.el ends here
