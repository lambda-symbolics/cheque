(defpackage #:cheque
  (:use #:cl)
  (:import-from #:serapeum
                #:->
                #:partial)
  (:documentation "Source rules, safe layout repairs, Mallet presets, and clean-compile warning checks.")
  (:export
   ;; Projects
   #:project
   #:project-root
   #:project-read
   #:project-preset
   #:*project*
   #:*project-presets*
   #:project-configuration-error
   ;; Files, findings, and rules
   #:source-file
   #:source-file-create
   #:source-file-path
   #:source-file-text
   #:source-finding
   #:source-finding-rule
   #:source-finding-path
   #:source-finding-line
   #:source-finding-column
   #:source-finding-message
   #:source-rule
   #:source-rule-name
   #:source-rule-description
   #:source-rule-applies-p
   #:source-rule-prepare
   #:source-rule-findings
   #:source-rule-fix
   #:source-rule-layout-only-p
   ;; Running checks
   #:source-check-rules
   #:source-check-run
   #:source-check-repair
   #:source-check-report
   #:source-check-unsafe-repair
   #:source-parinfer-library
   #:source-parinfer-library-ensure
   #:source-warning-run
   #:source-warning-run-start
   #:source-warning-run-process
   #:source-warning-run-finish
   #:source-warning-run-failed
   #:cheque-error
   #:cheque-error-message
   #:main))
