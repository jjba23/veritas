;; veritas
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(use-modules (guix packages)
             (gnu)
             (guix utils)
             (guix git-download)
             (guix download)
             (gnu packages guile)
             (gnu packages nss)
             (gnu packages code)
             (gnu packages guile-xyz)
             (gnu packages pkg-config)
             (guix build-system guile)
             (gnu packages linux)
             (gnu packages compression)
             (gnu packages texinfo)
             (gnu packages admin)
             (gnu packages emacs)
             (gnu packages rust-apps)
             (gnu packages autotools)
             ((guix licenses)
              #:prefix license:))

(packages->manifest (list guile-3.0-latest
                          guile-fibers
                          guile-json-4
                          guile-ares-rs
                          guile-documenta
                          texinfo
                          lcov
                          emacs-no-x
                          watchexec))
