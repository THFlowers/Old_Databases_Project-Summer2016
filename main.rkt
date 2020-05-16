#lang racket
(require web-server/servlet-env)
(require web-server/formlets web-server/formlets/syntax)
(require web-server/http)
(require web-server/http/cookie web-server/http/id-cookie)
(require web-server/page)
(require web-server/dispatch)
(require db)
(require openssl/sha1)
(require racket/system)

; Constants
(define comic-grid-rows 2)
(define comic-grid-columns 5)
(define comic-grid-size (* comic-grid-rows comic-grid-columns))
(define comic-thumbnail-height 150)
(define comic-thumbnail-width 84)

(define max-failed-logins 5)
(define domain-name "CosmonautKittens.com:8080")
(define working-dir "C:/Users/Thai Flowers/Desktop/ComicShop")
(define cygwin-bin-path "C:\\cygwin64\\bin")
(define secret-salt (make-secret-salt/file "current.salt"))
(define state-abbreviations (list->set
                             '("AL" "AK" "AZ" "AR" "CA" "CO" "CT" "DE" "DC" "FL" "GA" "HI" "ID" "IL" "IN"
                               "IA" "KS" "KY" "LA" "ME" "MD" "MA" "MI" "MN" "MS" "MO" "MT" "NE" "NV" "NH"
                               "NJ" "NM" "NY" "NC" "ND" "OH" "OK" "OR" "PA" "RI" "SC" "SD" "TN" "TX" "UT"
                               "VT" "VA" "WA" "WV" "WI" "WY" "AS" "GU" "MP" "PR" "VI" "UM")))

(define (database-login)
  (let ((password (begin (display "User: Ron \nDatabase: ComicShop \nPassword: ")
                         (read-line (current-input-port) 'any))))
    (virtual-connection ; convience of a singular connection, isolation of per thread connections
     (connection-pool ; convience of per thread connections, minus the overhead
      (lambda () (mysql-connect #:user "Ron" ; actual connection must be a thunk for above
                              #:database "ComicShop"
                              #:password password))))))
(define db-conn (database-login))

(define (date-select-formlet [value ""]) ; value is sql-date
 (formlet
  (div
   (style ((type "text/css"))
          "
          .ds_box {
	          background-color: #FFF;
	          border: 1px solid #000;
	          position: absolute;
	          z-index: 32767;
          }
          
          .ds_tbl {
	          background-color: #FFF;
          }
          
          .ds_head {
	          background-color: #333;
	          color: #FFF;
	          font-family: Arial, Helvetica, sans-serif;
	          font-size: 13px;
	          font-weight: bold;
	          text-align: center;
	          letter-spacing: 2px;
          }
          
          .ds_subhead {
	          background-color: #CCC;
	          color: #000;
	          font-size: 12px;
	          font-weight: bold;
	          text-align: center;
	          font-family: Arial, Helvetica, sans-serif;
	          width: 32px;
          }
          
          .ds_cell {
	          background-color: #EEE;
	          color: #000;
	          font-size: 13px;
	          text-align: center;
	          font-family: Arial, Helvetica, sans-serif;
	          padding: 5px;
	          cursor: pointer;
          }
          
          .ds_cell:hover {
	          background-color: #F3F3F3;
          } /* This hover code won't work for IE */
          "
          )
   (table ((class "ds_box") (cellpadding "0") (cellspacing "0") (id "ds_conclass") (style "display: none;"))
          (tr (td ((id "ds_calclass")))))
   (script ((src "/date-selector.js")))
   ,[(to-string (required (input
                           #:value (if (not (or (equal? value "") (sql-null? value)))
                                       (sql-date->selector-date value)
                                       "")
                           #:attributes `((onclick "ds_sh(this);")
                                          (readonly "readonly")
                                          (style "cursor: text"))
                           ))) . => . chosen_date]
   )
  chosen_date))
        

; General Helper Functions

; returns a 40 char long hash (via sha1, hex as string) of the input string str
(define (sha1-hash/string str)
  (bytes->hex-string (sha1-bytes (open-input-string str)))) ; input-strings don't need to be explicitly closed

(define (hash-passwords) ; saved for posterity or possible future use, hash all passwords (asumming they aren't already)
  (map (lambda (tup)(query-exec db-conn
                                "update Users set password=? where id=?"
                                (sha1-hash/string (vector-ref tup 1))
                                (vector-ref tup 0)))
       (query-rows db-conn "select id,password from Users")))

; takes a phone-number encoded as a string and returns the same minus any non-numeric characters
(define (sanitize-phone str) (list->string (filter char-numeric? (string->list str))))

(define (selector-date->sql-date str) ; yyyy-mm-dd format to struct
  (let ((parts (string-split str "-")))
    (sql-date (string->number (first parts))
              (string->number (second parts))
              (string->number (third parts)))))

(define (sql-date->selector-date date) ; struct to yyyy-mm-dd format
  (format "~a-~a-~a"
          (sql-date-year date)
          (sql-date-month date)
          (sql-date-day date)))

(define (sql-date-output date)
  (format "~a/~a/~a"
          (sql-date-month date)
          (sql-date-day date)
          (sql-date-year date)))

(define (double->dollar dbl)
  (let ((decimal-part? ((exact-truncate (abs dbl)) . < . (abs dbl)))
        (exact-part (exact-truncate (abs dbl))))
    (format "~a$~a~a~a"
          (if (equal? (inexact->exact (sgn dbl)) -1)
              "-"
              "")
          (exact-truncate (abs dbl))
          (if decimal-part?
              "."
              "")
          (if decimal-part?
              (~a #:width 2 #:pad-string "0" #:align 'right
                  (* (- (abs dbl) exact-part) 100))
              ""))))

; translate the single value byte-list interface for checkboxes to boolean
(define (bl->sql-bool bl)
  (if (equal? bl '(#"1"))
      1
      0))

(define (get-image-path series-name series-vol extension [absolute? #t])
  (format "~a/data/images/~a~a.~a"
          (if absolute? working-dir "")
          (string-replace (string-replace series-name " " "_") ":" "")
          (~a #:width 2 #:pad-string "0" #:align 'right
              series-vol)
          extension))


; Main HTML/Webserver Functions

(define-values (main-dispatch url)
  (dispatch-rules
   [("") main-page]
   [("products" (integer-arg)) main-page]
   [("products" (integer-arg) (string-arg)) main-page]
   [("details" (integer-arg)) comic-detail-page]
   [("contact") contact-page]
   [("log-in") login-page]
   [("log-out") logout-handler]
   [("new-user") new-user-page]
   [("new-user" (integer-arg)) new-user-page]
   [("cart") cart-page]
   [("search" (string-arg)) search-page]
   [("search" (string-arg) (integer-arg)) search-page]
   [("your-settings") your-settings-page]
   [("your-subs") your-sub-page]
   [("unsub" (integer-arg)) unsub-confirm]
   [("password-reset") password-reset-page]
   [("password-reset" (string-arg)) password-reset-page]
   [("shipping-rates") shipping-rates-page]
   [("shipping-rates" (integer-arg)) shipping-rates-page] ; int refers to return page (default 1) 1->main 2->your-settings
   [("admin-tools") admin-tools-page]
   [("min-order-report") min-order-page]
   [("admin-comics-report") admin-comics-page]
   [("admin-comic-delete" (integer-arg)) admin-comic-delete]
   [("admin-subscription-report" (integer-arg)) admin-subscription-page]
   [("admin-edit-series" (integer-arg)) admin-edit-series-page]
   [("admin-phys-users") admin-physical-users-page]
   [("admin-phys-sub-report" (integer-arg)) admin-phys-sub-report-page]
   [("admin-edit-phys-user" (integer-arg)) admin-edit-phys-user]
   [("admin-delete-phys-user" (integer-arg)) admin-delete-phys-user-page]
   [("admin-users") admin-users-page]
   [("admin-user-sub-report" (integer-arg)) admin-user-sub-report-page]
   [("admin-publishers") admin-publishers-page]
   ;[("admin-publisher-report" (integer-arg)) admin-publisher-report-page]
   [("admin-edit-publisher" (integer-arg)) admin-edit-publisher-page]
   [("admin-delete-publisher" (integer-arg)) admin-delete-publisher-page]
   ))

;originally used with requests, now handles urls and sends to main-dispatch
;which constructs full requests
;May fail if it recieves a proper request!
(define (start url)
      (main-dispatch url))

(define my-footer
  `(footer (div ((style "bottom: 0; width: 100%; font-size: small;"))
            (hr)
           "© Cosmonaut Kittens LLC" (br)
           (a ((href "/contact")) "contact us"))))

; take a list of column names and a list of rows (as vectors)
; produce a bland html table based on that data
; if a cell is a list, then return that list, else format as text
(define (generic-report-table column-headers rows [vector-process (lambda (v) v)] [width "100%"] [height ""] [align "center"])
  `(table ((width ,width) (align ,align) (border "3") (cellpadding "5") (cellspacing "0")
                          ,(if (not (equal? height ""))
                             `(height ,height)
                             `(dummy "dummy")))
          (thead (tr
                  ,@(map (lambda (col)
                           `(th ((align "center")) ,col))
                         column-headers)))
          (tbody
           ,@(map (lambda (row)
                    `(tr ,@(map (lambda (item)
                                  (if (list? item)
                                      `(td ,item)
                                      `(td ,(format "~a" item))))
                                (vector->list (vector-process row)))))
                  rows))
          ))

; Display the grid of available web pull-list products.
; columns and rows is preferred gride size,
; when there aren't enough items to fill grid, then empty div's will be produced
(define (comic-grid [page 1] [category "all"] [rows comic-grid-rows] [columns comic-grid-columns])
  (letrec ((page-size (* rows columns))
           (comics (list->vector
                    (if (equal? category "all")
                        (query-rows db-conn "SELECT id,name,volume,ongoing_rate,six_issue_rate
                                             FROM Web_Subscribable
                                             ORDER BY name LIMIT ? OFFSET ?"
                                    page-size ; total number of items in grid, # needed to be fetched from query
                                    (* (if (page . <= . 1) ; if page 1 offset of 0, else offset of page-size * (page - 1)
                                           0
                                           (- page 1))
                                       page-size))
                        (query-rows db-conn "SELECT id,name,volume,ongoing_rate,six_issue_rate
                                             FROM Web_Subscribable
                                             WHERE publisher=(SELECT id
                                                              FROM   Publisher
                                                              WHERE  name=?)
                                             ORDER BY name LIMIT ? OFFSET ?"
                                    (string-replace category "_" " ")
                                    page-size
                                    (* (if (page . <= . 1)
                                           0
                                           (- page 1))
                                       page-size))
                        )))
           (num-comics (vector-length comics)))
  `(table ((width "100%") (align "center") (border "0") (cellpadding "5") (cellspacing "0"))
          (tbody
           ,@(map (lambda (row)
                    `(tr ((valign "top") (align "center"))
                         (td ((width "25%"))
                             (table ((width "99%") (border "0") (cellpadding "5") (cellspacing "0"))
                                     (tbody
                                      (tr
                                       ,@(map (lambda (col)
                                                      (letrec ((current-position (+ (* row columns) col))
                                                               (comic (if (current-position . < . num-comics)
                                                                          (vector-ref comics current-position)
                                                                          #f))
                                                               (comic-name (if comic (vector-ref comic 1) #f))
                                                               (cover-image (if (and comic
                                                                                     (file-exists?
                                                                                      (format "C:/Users/Thai Flowers/Desktop/ComicShop/data/images/~a~a_thumb.png"
                                                                                              (string-replace (string-replace comic-name " " "_") ":" "")
                                                                                              (~a #:width 2 #:pad-string "0" #:align 'right
                                                                                                  (vector-ref comic 2)))))
                                                                                (format "/data/images/~a~a_thumb.png"
                                                                                        (string-replace (string-replace comic-name " " "_") ":" "")
                                                                                        (~a #:width 2 #:pad-string "0" #:align 'right
                                                                                            (vector-ref comic 2)))
                                                                                "/data/images/placeholder2.png")))
                                                        (if comic
                                                            `(td ((height "150") (align "center"))
                                                               (table ((bgcolor "white"))
                                                                (tr (div ((style "overflow: hidden; height: 150px;"))
                                                                         (a ((href ,(format "/details/~a" (vector-ref comic 0))))
                                                                            (img ((src ,cover-image)
                                                                                  (alt ,comic-name))))))
                                                                (tr (div ((style "overflow: hidden; width: 150px"))
                                                                         ,comic-name))))
                                                            `(td ((height "150") (align "center"))
                                                                 (div ((style "overflow: hidden; height: 150px; width: 150px"))
                                                                              )))))
                                                      (stream->list (in-range 0 columns)))
                                               ))))))
                    (stream->list (in-range 0 rows)))
           ))))

(define (search-formlet-gen [str "Search"])
         (formlet
          (div
           (script ((type "text/javascript"))
                   "
                   function SelectAll(id)
                   {
                        document.getElementById(id).focus();
                        document.getElementById(id).select();
                   }
                   ")
           ,[(to-string (required (text-input #:value str #:max-length 80 #:size 20
                                              #:attributes `((id "search") (onclick "SelectAll('search')"))
                                              ))) . => . search-string])
          (redirect-to (format "/search/~a" search-string))))

; add proper limited series support during polish
; also replace buggy radio-group with selection box
(define (comic-detail-page req series-id)
  (letrec ((comic (query-maybe-row db-conn
                                "SELECT *
                                FROM Web_Subscribable
                                WHERE id=?"
                                series-id))
        (comic-name (if (equal? comic false)
                        #f
                        (vector-ref comic 1)))
        (user-id (request-id-cookie "logged-in"
                                       secret-salt
                                       req))
        (sub-data (if (not (equal? user-id false))
                         (query-maybe-row db-conn
                                            "SELECT issues_left,cart
                                             FROM Pull_List
                                             WHERE user_id=?
                                               AND series_id=?"
                                            user-id series-id)
                         #f))
        (issues-left (if (not (equal? sub-data false))
                         (vector-ref sub-data 0)
                         #f))
        (in-cart (if (not (equal? sub-data false))
                     ((vector-ref sub-data 1) . > . 0)
                     #f))
        (sub-radio-formlet (formlet ; blows up when no option select, no way to fix without hacking language itself and won't accept defaults! FUCK YOU RACKET!
                            (div ,[(radio-group (list 1 2)
                                                #:display (lambda (i) (if (= i 1)
                                                                          "6-issues"
                                                                          "Ongoing")))
                                   . => . choice])
                            (begin (if (equal? choice 1)
                                       (query-exec db-conn "INSERT INTO Pull_List VALUES
                                                            (?,?,6,?,CURDATE(),True)"
                                                   user-id series-id (vector-ref comic 5))
                                       (query-exec db-conn "INSERT INTO Pull_List VALUES
                                                            (?,?,NULL,?,CURDATE(),True)"
                                                   user-id series-id (vector-ref comic 4)))
                                   (redirect-to (format "/details/~a" series-id))))))
    (page
     (if (equal? false comic)
      (response/xexpr
       `(html (head (title "Invalid Comic"))
              (body (h1 "Error: Comic not found")
                    (a ((href "/")) "Return to main page"))))
      (response/xexpr
       `(html (script ((type "text/javascript"))
                      "
                       function getImgSize(imgSrc) {
                         var newImg = new Image();
                         newImg.src = imgSrc;
                         return [newImg.width,newImg.height];
                       }
                       var flag = true;
                       function resize() {
                         var my_image = document.getElementById(\"pic\");
                         var max_dim = getImgSize(my_image.src);
                         if(flag) {
		                 my_image.style.width = \"500px\";
		                 my_image.style.height = \"725px\";
	                       } else {
	                         my_image.style.width = \"1000px\";
	                         my_image.style.height = \"1000px\";
                           flag=true;
	                       }
                       }
                       ")
              (head (title ,(format "~a Details" comic-name)))
              (body ((bgcolor "lightyellow") (text "orange"))
               (a ((href "/")) "Return to main page")
               (table ((width "50%") (align "center")) 
                (tr ((align "center")) (td (h1 ,comic-name)))
                (tr ((align "center"))
                    (td (img ((id "pic")
                             (src
                               ,(if (file-exists?
                                     (format "C:/Users/Thai Flowers/Desktop/ComicShop/data/images/~a~a.png"
                                             (string-replace (string-replace comic-name " " "_") ":" "")
                                             (~a #:width 2 #:pad-string "0" #:align 'right
                                                 (vector-ref comic 2))))
                                    (format "/data/images/~a~a.png"
                                            (string-replace (string-replace comic-name " " "_") ":" "")
                                            (~a #:width 2 #:pad-string "0" #:align 'right
                                                (vector-ref comic 2)))
                                    "/data/images/placeholder2.png"))
                               (alt ,comic-name)
                               (style "height:725px;width: expression(this.width > 500 ? 500: true);")
                               ;(onclick "resize();")
                               ))))
                (tr ((align "center"))
                    (td (table ((border "2") (cellspacing "0") (cellpadding "5"))
                         (thead (tr
                          (th "Six Month Price")
                          (th "Ongoing Price")))
                         (tbody (tr
                          (td ,(format "~a/issue" (double->dollar (vector-ref comic 5))))
                          (td ,(format "~a/issue" (double->dollar (vector-ref comic 4)))))))))
                (tr ((align "center"))
                    (td
                     ,(if (not (equal? user-id false)) ; user logged in
                           (if (equal? issues-left false) ; no subscription
                               `(form ([action ,(embed/url (lambda (inner-req)
                                                             (with-handlers ([exn:fail?
                                                                              (lambda (exn)
                                                                                   (begin
                                                                                     (query-exec db-conn
                                                                                               "INSERT INTO Pull_List VALUES
                                                                                                (?,?,NULL,?,CURDATE(),True)"
                                                                                               user-id series-id (vector-ref comic 4))
                                                                                     (redirect-to (format "/details/~a" series-id))))])
                                                                 (formlet-process sub-radio-formlet inner-req))))])
                                      ,@(formlet-display sub-radio-formlet)
                                      (input ([type "submit"] [value "Add to cart"])))
                               `(div ((align "center"))
                                     ,(if in-cart
                                         `(h1 "In Cart")
                                         `(h1 "Subscribed"))
                                      ,(if (sql-null? issues-left)
                                          `(h2 "Ongoing")
                                          `(h2 ,(format "~a Issues Left" issues-left)))))
                           '())
                      )))
                )))))))

; no continuation used so a direct response can be generated
(define (main-page req [pg 1] [category "all"])
  (letrec
      ((logged-in? (request-id-cookie "logged-in"
                                       secret-salt
                                       req))
       (admin? (if logged-in?
                   (= 1 (query-value db-conn "SELECT admin FROM Users WHERE id=?" logged-in?))
                   #f)) ; logged-in? == user-id
       (total-items (if (equal? category "all")
                        (query-maybe-value db-conn "select count(id) from Web_Subscribable")
                        (query-maybe-value db-conn "select count(id) from Web_Subscribable
                                                    where publisher=(SELECT id FROM publisher WHERE name=?)"
                                           (string-replace category "_" " "))))
       (num-pages (ceiling (/ total-items comic-grid-size)))
       (search-formlet (search-formlet-gen))
       )
    (page
    (response/xexpr
     `(html 
            (head (title "Cosmonaut Kittens Comic Shop"))
            (body ((bgcolor "lightyellow") (text "orange"))
                (h1 "Cosmonaut Kittens Comic Shop")
                ,(if (not logged-in?)
                     `(table ((width "100%"))
                             (tr (td ((align "left")) (a ((href "/log-in")) "Log-in to Pull-List Management"))))
                     `(table ((width "100%"))
                             (tr
                              (td ((align "left"))  (a ((href "/log-out")) "Log-out of Pull-List management"))
                              ,(if admin?
                                 `(td ((align "left"))  (a ((href "/admin-tools")) "Admin Tools"))
                                 `(td))
                              (td ((align "right")) (a ((href "/your-settings")) "Change Settings"))
                              (td ((align "right")) (a ((href "/your-subs")) "View Your Subscriptions"))
                              (td ((align "right")) (a ((href "/cart")) "Manage Cart")))))
                (hr)
                (table ((width "100%") (align "center"))
                 (tr
                  (td ((height "100%"))
                      (table ((height "100%") (style "border-colapse: colapse;"))
                       (tr
                        (td ((valign "bottom"))
                            (form ([action ,(embed/url
                                             (lambda (inner-req)
                                               (formlet-process search-formlet inner-req)))])
                                  ,@(formlet-display search-formlet)
                                  (input ([type "submit"] [value "submit"])))))
                       (tr
                        (td
                         ,(generic-report-table '("Categories")
                                                (query-rows db-conn "SELECT 'all' AS name
                                                                     UNION
                                                                     SELECT name FROM Publisher WHERE id IN (SELECT publisher FROM Web_Subscribable)")
                                                (lambda (vec)
                                                  (begin (vector-set! vec 0 `(a ((href ,(format "/products/1/~a"
                                                                                                (string-replace (vector-ref vec 0) " " "_"))))
                                                                                ,(vector-ref vec 0)))
                                                         vec))
                                                "100%"
                                                "100%")))))
                  (td
                   (table ((width "95%") (align "center"))
                          (tr
                           (td ((align "left"))
                               (h3 ,(format "Products (Total Items: ~a)"
                                            total-items)))
                           (td ((align "right"))
                               (table ((with "100%") (cellpadding "5"))
                                      (tr
                                       ,(if (and (pg . > . 1) (num-pages . > . 1))
                                            `(td ((align "right"))
                                                 (a ((href ,(if (not (equal? category "all"))
                                                                (format "/products/~a/~a"
                                                                        (- pg 1)
                                                                        category)
                                                                (format "/products/~a"
                                                                        (- pg 1)))))
                                                    (h3 "prev")))
                                            `(td))
                                       ; Assumes small number of products pages, modify later
                                       ,@(if (num-pages . > . 1)
                                             (map (lambda (num)
                                                    `(td ((align "right"))
                                                         (h3 ,(if (= num pg)
                                                                  `(b ,(format "[~a]" num))
                                                                  `(a ((href ,(format "/products/~a/~a" num category)))
                                                                      ,(format "[~a]" num))))))
                                                  (stream->list (in-range 1 (+ 1 num-pages))))
                                             '())
                                       ,(if (pg . < . num-pages)
                                            `(td ((align "right"))
                                                 (a ((href ,(if (not (equal? category "all"))
                                                                (format "/products/~a/~a" (+ pg 1) category)
                                                                (format "/products/~a" (+ pg 1)))))
                                                    (h3 "next")))
                                            `(td))))))
                          (tr (td ((colspan "2"))
                                  ,(comic-grid pg category)))))))
                )
          ,my-footer
          )))))

; 1 Function Multiple pages, communicate id back and forth via cookies
; add new_user table of unfinished id and start_time for deletint unfinished entires at regular intervals
; page 1) id,user_name,password
(define (new-user-page req [pg 1])
  (letrec ((new-user-id (request-id-cookie "new-user"
                                           secret-salt
                                           req))
           (page-1-formlet (formlet
                            (div
                             "Username:         " ,[(to-string (required (text-input #:max-length 50))) . => . user_name]
                             (br)
                             "Password:         " ,[(to-string (required (password-input))) . => . password_1]
                             (br)
                             "Confirm password: " ,[(to-string (required (password-input))) . => . password_2]
                             (br)
                             "email:            " ,[(to-string (required (text-input #:max-length 128))) . => . email_1]
                             (br)
                             "Confirm email:    " ,[(to-string (required (text-input #:max-length 128))) . => . email_2]
                             (br)
                             )
                            (if ((string-length user_name) . < . 5)
                                (response/xexpr `(html (head (title "Username too short"))
                                                       (body (h1 "Username too short, at least 5 characters required")
                                                             (a ((href "/new-user/1")) "Try again"))))
                                (if (equal? password_1 "")
                                    (response/xexpr `(html (head (title "Blank password"))
                                                           (body (h1 "Non-blank password required!")
                                                                 (a ((href "/new-user/1")) "Try again"))))
                                    (if (not (regexp-match #rx".*@.*" email_1))
                                        (response/xexpr `(html (head (title "Invald email format"))
                                                               (body (h1 "Invalid email format, input like User@Example.com")
                                                                     (a ((href "/new-user/1")) "Try again"))))
                                        (if (query-maybe-value db-conn "SELECT user_name FROM Users WHERE user_name=?" user_name)
                                            (response/xexpr `(html (head (title "Username taken"))
                                                                   (body (h1 "Username taken")
                                                                         (a ((href "/new-user/1")) "Try again"))))
                                            (if (not (equal? password_1 password_2))
                                                (response/xexpr `(html (head (title "Password Mismatch"))
                                                                       (body (h1 "Password Not confirmed")
                                                                             (a ((href "/new-user/1")) "Try again"))))
                                                (if (not (equal? email_1 email_2))
                                                    (response/xexpr `(html (head (title "Email Mismatch"))
                                                                           (body (h1 "Email Not confirmed")
                                                                                 (a ((href "/new-user/1")) "Try again"))))
                                                    (call-with-transaction db-conn
                                                     (lambda ()
                                                       (begin (query-exec db-conn
                                                                       "INSERT INTO New_Users (id,user_name,password,email) VALUES
                                                                        (DEFAULT,?,?,?)"
                                                                       user_name (sha1-hash/string password_1) email_1)
                                                           (redirect-to "/new-user/2"
                                                                        see-other
                                                                        #:headers (list (cookie->header (make-id-cookie
                                                                                                         "new-user"
                                                                                                         secret-salt
                                                                                                         (number->string (query-value db-conn
                                                                                                                                      "SELECT LAST_INSERT_ID()"
                                                                                                                                      ))))))
                                                           )))
                                                    ))))))
                            ))
           (page-2-formlet (formlet
                            (div
                             "First Name: " ,[(to-string (required (text-input #:max-length 128 #:size 30))) . => . first_name]
                             "Last Name:  " ,[(to-string (required (text-input #:max-length 128 #:size 30))) . => . last_name]
                             (br)
                             (h2 "Shipping Address")
                             "Address:    " ,[(to-string (required (text-input #:max-length 128 #:size 40))) . => . address_1]
                             (br)
                             "Address 2:  " ,[(to-string (required (text-input #:max-length 128 #:size 40))) . => . address_2]
                             (br)
                             "Address 3:  " ,[(to-string (required (text-input #:max-length 128 #:size 40))) . => . address_3]
                             (br)
                             "City:       " ,[(to-string (required (text-input #:max-length 128 #:size 30))) . => . city]
                             "     State: " ,[(to-string (required (text-input #:max-length 2   #:size 2 ))) . => . state]
                             "       Zip: " ,[(to-string (required (text-input #:max-length 10  #:size 5 ))) . => . zip]
                             )
                            (if (equal? first_name "")
                                (response/xexpr `(html (head (title "Missing First Name"))
                                                       (body (h1 "You left \"First Name\" blank")
                                                             (a ((href "/new-user/2")) "Try again"))))
                                (if (equal? first_name "")
                                    (response/xexpr `(html (head (title "Missing Last Name"))
                                                           (body (h1 "You left \"Last Name\" blank")
                                                                 (a ((href "/new-user/2")) "Try again"))))
                                    (if (equal? address_1 "")
                                        (response/xexpr `(html (head (title "Missing Address"))
                                                               (body (h1 "You left \"Address\" blank")
                                                                     (a ((href "/new-user/2")) "Try again"))))
                                        (if (equal? city "")
                                            (response/xexpr `(html (head (title "Missing City"))
                                                                   (body (h1 "You left \"City\" blank")
                                                                         (a ((href "/new-user/2")) "Try again"))))
                                            (if (not (set-member? state-abbreviations
                                                                  (list->string
                                                                   (map char-upcase (string->list state)))))
                                                (response/xexpr `(html (head (title "Not a valid state"))
                                                                       (body (h1 "Not a valid state abbreviation")
                                                                             (a ((href "/new-user/2")) "Try again"))))
                                                (if (not (or (regexp-match #rx"....." zip)
                                                             (regexp-match #rx".....-...." zip)))
                                                    (response/xexpr `(html (head (title "Bad zip format"))
                                                                           (body (h1 "Zip code not in 5 digit or 5+4 format")
                                                                                 (a ((href "/new-user/2")) "Try again"))))
                                                    (call-with-transaction db-conn
                                                      (lambda ()
                                                        (begin (query-exec db-conn
                                                                           "INSERT INTO Address VALUES
                                                                            (DEFAULT,'New User Addr',?,?,?,?,?,?)"
                                                                           address_1 address_2 address_3 city state zip)
                                                               (query-exec db-conn
                                                                           "UPDATE New_Users
                                                                            SET first_name=?,last_name=?,address=LAST_INSERT_ID()
                                                                            WHERE id=?"
                                                                           first_name last_name new-user-id)
                                                               (redirect-to "/new-user/3")
                                                               )))
                                                    ))))))
                            ))
           (page-3-formlet (formlet
                            (div
                             (h2 "Billing Address")
                             "Same as shipping address?" ,[(default #"0" (checkbox #"1" #t)) . => . same?]
                             (br)
                             "Address:    " ,[(to-string (required (text-input #:max-length 128 #:size 40))) . => . address_1]
                             (br)
                             "Address 2:  " ,[(to-string (required (text-input #:max-length 128 #:size 40))) . => . address_2]
                             (br)
                             "Address 3:  " ,[(to-string (required (text-input #:max-length 128 #:size 40))) . => . address_3]
                             (br)
                             "City:       " ,[(to-string (required (text-input #:max-length 128 #:size 30))) . => . city]
                             "     State: " ,[(to-string (required (text-input #:max-length 2   #:size 2 ))) . => . state]
                             "       Zip: " ,[(to-string (required (text-input #:max-length 10  #:size 5 ))) . => . zip]
                             )
                            (if (equal? same? #"1") ; billing address is same as shipping address, that is billing_address=NULL
                                (call-with-transaction db-conn
                                  (lambda ()
                                    (begin ;(query-exec db-conn "UPDATE New_Users SET billing_address=NULL WHERE id=?" new-user-id)
                                           (apply (lambda (user-name address-id)
                                                    (query-exec db-conn "UPDATE Address SET name=? WHERE id=?"
                                                                (format "User ~a Addr" user-name) address-id))
                                                  (vector->list (query-row db-conn "SELECT user_name,address FROM New_Users WHERE id=?" new-user-id)))
                                           (query-exec db-conn "INSERT INTO Users
                                                               (user_name,password,email,first_name,last_name,address,billing_address)
                                                               SELECT user_name,password,email,first_name,last_name,address,billing_address
                                                               FROM   New_Users
                                                               WHERE id=?" new-user-id)
                                           (let ((user-id (query-value db-conn "SELECT LAST_INSERT_ID()")))
                                             (query-exec db-conn "INSERT INTO Web_Transactions VALUES
                                                                (?,NOW(),CONCAT('New User ',(SELECT user_name
                                                                                             FROM   Users
                                                                                             WHERE  id=?)),0)"
                                                         user-id user-id)
                                             (query-exec db-conn "DELETE FROM New_Users WHERE id=?" new-user-id)
                                             (redirect-to "/" #:headers (list (cookie->header (logout-id-cookie "new-user"))
                                                                              (cookie->header (make-id-cookie
                                                                                              "logged-in"
                                                                                              secret-salt
                                                                                              (number->string user-id)
                                                                                              #:path "/"))))
                                           ))))
                                (if (equal? address_1 "")
                                        (response/xexpr `(html (head (title "Missing Address"))
                                                               (body (h1 "You left \"Address\" blank")
                                                                     (a ((href "/new-user/3")) "Try again"))))
                                        (if (equal? city "")
                                            (response/xexpr `(html (head (title "Missing City"))
                                                                   (body (h1 "You left \"City\" blank")
                                                                         (a ((href "/new-user/3")) "Try again"))))
                                            (if (not (set-member? state-abbreviations
                                                                  (list->string
                                                                   (map char-upcase (string->list state)))))
                                                (response/xexpr `(html (head (title "Not a valid state"))
                                                                       (body (h1 "Not a valid state abbreviation")
                                                                             (a ((href "/new-user/3")) "Try again"))))
                                                (if (not (or (regexp-match #rx"....." zip)
                                                             (regexp-match #rx".....-...." zip)))
                                                    (response/xexpr `(html (head (title "Bad zip format"))
                                                                           (body (h1 "Zip code not in 5 digit or 5+4 format")
                                                                                 (a ((href "/new-user/3")) "Try again"))))
                                                    (call-with-transaction db-conn
                                                      (lambda ()
                                                        (begin (apply (lambda (user-name address-id)
                                                                        (query-exec db-conn "UPDATE Address SET name=? WHERE id=?"
                                                                                    (format "User ~a Addr" user-name) address-id))
                                                                      (vector->list (query-row db-conn
                                                                                               "SELECT user_name,address
                                                                                                FROM   New_Users
                                                                                                WHERE  id=?" new-user-id)))
                                                               (query-exec db-conn
                                                                           "INSERT INTO Address VALUES
                                                                            (DEFAULT,concat_ws(' ',
                                                                                               'User',
                                                                                               (SELECT user_name
                                                                                                  FROM New_Users
                                                                                                 WHERE id=?),
                                                                                                'Bill Addr')
                                                                                              ,?,?,?,?,?,?)"
                                                                           new-user-id
                                                                           address_1 address_2 address_3 city state zip)
                                                               (query-exec db-conn
                                                                           "UPDATE New_Users
                                                                            SET billing_address=LAST_INSERT_ID()
                                                                            WHERE id=?"
                                                                           new-user-id)
                                                               (query-exec db-conn
                                                                           "INSERT INTO Users
                                                                            (user_name,password,email,first_name,last_name,address,billing_address)
                                                                            SELECT user_name,password,email,first_name,last_name,address,billing_address
                                                                            FROM   New_Users
                                                                            WHERE  id=?" new-user-id)
                                                               (let ((user-id (query-value db-conn "SELECT LAST_INSERT_ID()")))
                                                                 (query-exec db-conn "INSERT INTO Web_Transactions VALUES
                                                                                   (?,NOW(),CONCAT('New User ',(SELECT user_name
                                                                                                                FROM   Users
                                                                                                                WHERE  id=?)),0)"
                                                                             user-id user-id)
                                                                 (query-exec db-conn "DELETE FROM New_Users WHERE id=?" new-user-id)
                                                                 (redirect-to "/" #:headers (list (cookie->header (logout-id-cookie "new-user"))
                                                                                                  (cookie->header (make-id-cookie
                                                                                                                  "logged-in"
                                                                                                                  secret-salt
                                                                                                                  (number->string user-id)
                                                                                                                  #:path "/"))))
                                                               ))))
                                                    )))))
                            ))
           (page->formlet (lambda (pg)
                            (match pg
                              [1 page-1-formlet]
                              [2 page-2-formlet]
                              [3 page-3-formlet]))))
    (page
     (response/xexpr
      `(html (head (title "Create New Account"))
             (body ((bgcolor "lightyellow") (text "orange"))
                   ,(if (not (or (0 . < . pg) (pg . <= . 3)))
                        `(div (h1 "Invalid page")
                              (a ((href "/"))
                                 "Return to main"))
                    `(div
                      (h1 "Create New Account")
                      (form ([action ,(embed/url
                                     (lambda (inner-req)
                                       (formlet-process (page->formlet pg) inner-req)))])
                             ,@(formlet-display (page->formlet pg))
                             (input ([type "submit"] [value "Next Page"]))))
                      ))
                ,my-footer
                ))
          )))

(define (cart-page req)
  (letrec ((user-id (request-id-cookie "logged-in"
                                       secret-salt
                                       req))
           (cart-rows (if user-id
                          (query-rows db-conn "SELECT name,rate_at_sub,issues_left,(issues_left*rate_at_sub), series_id
                                               FROM Pull_List,Web_Subscribable
                                               WHERE user_id=? AND cart=TRUE AND id=series_id
                                               ORDER BY name"
                                      user-id)
                          #f))
                          
           (num-ongoing (if (not (or (empty? cart-rows) (not user-id)))
                            (query-value db-conn "SELECT count(series_id)
                                                  FROM   Pull_List
                                                  WHERE  user_id=? AND cart=True
                                                    AND  issues_left is NULL"
                                         user-id)
                            #f))
           (prepay-total (if (not (or (empty? cart-rows) (not user-id)))
                             (query-value db-conn "SELECT IFNULL(sum(issues_left*rate_at_sub),0)
                                                   FROM   Pull_List
                                                   WHERE  user_id=? AND cart=True
                                                     AND  issues_left is NOT NULL"
                                          user-id)
                             #f)))
    (if (not user-id)
        (redirect-to "/")
        (page
         (response/xexpr
          `(html
            (head (title "Your Shopping Cart"))
            (body
             (a ((href ,"/")) "Return to main page")
             ,(if (not (empty? cart-rows))
                  `(div
                    (h1 "Review your shopping cart")
                    ,(generic-report-table '("Series", "Current Price", "Number of issues", "Prepay Cost", "Delete?")
                                           cart-rows
                                           (lambda (vec)
                                             (begin (vector-set! vec 1 (format "~a/issue" (double->dollar (vector-ref vec 1))))
                                                 (when (sql-null? (vector-ref vec 2))
                                                   (vector-set! vec 2 "Ongoing"))
                                                 (vector-set! vec 3 (if (sql-null? (vector-ref vec 3))
                                                                        "N/A"
                                                                        (double->dollar (vector-ref vec 3))))
                                                 (vector-set! vec 4 ;`(a ((href ,(format "/unsub/~a" (vector-ref vec 4))))
                                                              (let ((series-id (vector-ref vec 4)))
                                                                `(form ([action ,(embed/url
                                                                                  (lambda (inner-req)
                                                                                    (begin (query-exec db-conn "DELETE FROM Pull_List
                                                                                                                WHERE user_id=?
                                                                                                                  AND series_id=?
                                                                                                                  AND cart=True"
                                                                                                       user-id series-id)
                                                                                           (redirect-to "/cart")))
                                                                                  )])
                                                                       (input ([type "submit"] [value "Delete"])))))
                                                 vec)))
                (table ((width "50%") (cellpadding "5") (align "right"))
                       (tr
                        (td (table (tr (td "Number Ongoing"))
                                   (tr (td ,(~a num-ongoing)))))
                        (td (table (tr (td "Prepay Total"))
                                   (tr (td ,(double->dollar (if (sql-null? prepay-total)
                                                                0
                                                                prepay-total))))))
                        (td (form ([action ,(embed/url
                                             (lambda (inner-req)
                                               (begin (query-exec db-conn "DELETE FROM Pull_List
                                                                            WHERE user_id=? AND cart=True"
                                                                  user-id)
                                                      (redirect-to "/cart"))))])
                                  (input ([type "submit"] [value "Clear"]))))
                        (td (form ([action ,(embed/url
                                             (lambda (inner-req)
                                               (call-with-transaction
                                                db-conn
                                                (lambda ()
                                                  (begin (query-exec db-conn "UPDATE Pull_List SET cart=False
                                                                            WHERE user_id=? AND cart=True"
                                                                     user-id)
                                                         (query-exec db-conn "UPDATE Users SET balance=balance+?
                                                                            WHERE id=?"
                                                                     prepay-total user-id)
                                                         (when (prepay-total . > . 0)
                                                           (query-exec db-conn "INSERT INTO Web_Transactions VALUES
                                                                             (?,DEFAULT,'Sub Pre-Pay',?)"
                                                                       user-id prepay-total))
                                                         (when (num-ongoing . > . 0)
                                                           (query-exec db-conn "INSERT INTO Web_Transactions VALUES
                                                                             (?,DEFAULT,?,0)"
                                                                       user-id (format "'~a New Ongoing Subs'" num-ongoing)))
                                                         (response/xexpr
                                                          `(html
                                                            (head (title "Payment form placeholder"))
                                                            (body (h1 "Payment form placeholder")
                                                                  (p "Pretend you just made a credit card payment via paypal")
                                                                  (a ((href "/cart")) "Click here to return")))))))
                                                      ))])
                                  (input ([type "submit"] [value "Check-out"]))))))
                )
              `(h1 "Cart is Empty"))
         )))))))

(define (search-page req str [pg 1])
  (letrec ((result-rows (query-rows db-conn
                                    "SELECT S.name,volume,P.name,S.id
                                     FROM   Web_Subscribable S, Publisher P
                                     WHERE  S.publisher=P.id
                                     AND    MATCH(S.name) AGAINST(concat(?,'*') IN BOOLEAN MODE)"
                        str))
           (num-results (length result-rows))
           (search-formlet (search-formlet-gen str)))
  (page
  (response/xexpr
   `(html (head (title "Search Results"))
          (body ((bgcolor "lightyellow") (text "orange"))
                (a ((href "/")) "Return to main")
                (form ([action ,(embed/url
                                 (lambda (inner-req)
                                   (formlet-process search-formlet inner-req)))])
                      ,@(formlet-display search-formlet)
                      (input ([type "submit"] [value "submit"])))
                ,(if (equal? '() result-rows)
                    `(h1 "No Results")
                    `(div
                      (h1 "Search Results")
                      ,(generic-report-table `("Name" "Volume" "Publisher" "more info")
                                             result-rows
                                             (lambda (vec)
                                               (begin (vector-set! vec 3`(a ((href ,(format "/details/~a"
                                                                                            (vector-ref vec 3))))
                                                                            "more"))
                                                      vec)))))
                )
          ,my-footer
          )))))

(define (your-settings-page req)
  (letrec ((user-id (request-id-cookie "logged-in"
                                       secret-salt
                                       req))
           (user-data (if user-id
                          (query-maybe-row db-conn "SELECT id,user_name,password,IFNULL(phone,''),email,
                                                           address,billing_address,shipping_schedule,shipping_class
                                                    FROM Users
                                                    WHERE id=?" user-id)
                          #f))
           (phone-formlet (if user-data
                              (formlet
                               (div
                                (h2 "Phone")
                                ,[(to-string (required (text-input #:value (vector-ref user-data 3)
                                                                   #:max-length 15
                                                                   #:size 15))) . => . phone])
                               (begin (query-exec db-conn "UPDATE Users SET phone=? WHERE id=?" (sanitize-phone phone) user-id)
                                      (redirect-to "/your-settings")))
                              #f))
           (email-formlet (if user-data
                              (formlet
                               (div
                                (h2 "Email")
                                "email"   ,[(to-string (required (text-input #:value (vector-ref user-data 4)
                                                                             #:max-length 128))) . => . email_1]
                                (br)
                                "confirm" ,[(to-string (required (text-input #:max-length 128))) . => . email_2])
                               (if (equal? email_1 (vector-ref user-data 4))
                                   (redirect-to "/your-settings")
                                   (if (not (equal? email_1 email_2))
                                       (response/xexpr `(html (head (title "Email Mismatch"))
                                                              (body (h1 "Email Not confirmed")
                                                                    (a ((href "/your-settings")) "Try again"))))
                                       (if (not (regexp-match #rx".*@.*" email_1))
                                           (response/xexpr `(html (head (title "Invald email format"))
                                                                  (body (h1 "Invalid email format, input like User@Example.com")
                                                                        (a ((href "/your-settings")) "Try again"))))
                                           (begin (query-exec "UPDATE Users
                                                               SET email=?,verified_email=False
                                                               WHERE id=?"
                                                              email_1 user-id)
                                                  (redirect-to "/your-settings"))))))
                              #f))
           (passw-formlet (if user-data
                              (formlet
                               (div
                                (h2 "Password")
                                "password" ,[(to-string (required (password-input))) . => . password_1]
                                (br)
                                "confirm"  ,[(to-string (required (password-input))) . => . password_2])
                               (if (equal? password_1 "")
                                   (response/xexpr `(html (head (title "Blank password"))
                                                          (body (h1 "Non-blank password required!")
                                                                (a ((href "/your-settings")) "Try again"))))
                                    (if (not (equal? password_1 password_2))
                                        (response/xexpr `(html (head (title "Password Mismatch"))
                                                               (body (h1 "Password Not confirmed")
                                                                     (a ((href "/your-settings")) "Try again"))))
                                        (begin (query-exec db-conn
                                                           "UPDATE Users
                                                            SET password=?
                                                            WHERE id=?"
                                                           (sha1-hash/string password_1) user-id)
                                                  (redirect-to "/your-settings")))))
                              #f))
           (addr-formlet (if user-data
                             (let ((address (query-row db-conn "SELECT * FROM Address WHERE id=?" (vector-ref user-data 5))))
                               (formlet
                                (div
                                 (h2 "Shipping Address")
                                 "Address:    " ,[(to-string (required (text-input #:max-length 128 #:value (vector-ref address 2)))) . => . address_1]
                                 (br)
                                 "Address 2:  " ,[(to-string (required (text-input #:max-length 128 #:value (vector-ref address 3)))) . => . address_2]
                                 (br)
                                 "Address 3:  " ,[(to-string (required (text-input #:max-length 128 #:value (vector-ref address 4)))) . => . address_3]
                                 (br)
                                 "City:       " ,[(to-string (required (text-input #:max-length 128 #:value (vector-ref address 5)))) . => . city]
                                 "State:      " ,[(to-string (required (text-input #:max-length 2   #:size 2 #:value (vector-ref address 6)))) . => . state]
                                 "Zip:        " ,[(to-string (required (text-input #:max-length 10  #:size 5 #:value (vector-ref address 7)))) . => . zip]
                                 )
                                (if (equal? address_1 "")
                                    (response/xexpr `(html (head (title "Missing Address"))
                                                           (body (h1 "You left \"Address\" blank")
                                                                 (a ((href "/your-settings")) "Try again"))))
                                    (if (equal? city "")
                                        (response/xexpr `(html (head (title "Missing City"))
                                                               (body (h1 "You left \"City\" blank")
                                                                     (a ((href "/your-settings")) "Try again"))))
                                        (if (not (set-member? state-abbreviations
                                                              (list->string
                                                               (map char-upcase (string->list state)))))
                                            (response/xexpr `(html (head (title "Not a valid state"))
                                                                   (body (h1 "Not a valid state abbreviation")
                                                                         (a ((href "/your-settings")) "Try again"))))
                                            (if (not (or (regexp-match #rx"....." zip)
                                                         (regexp-match #rx".....-...." zip)))
                                                (response/xexpr `(html (head (title "Bad zip format"))
                                                                       (body (h1 "Zip code not in 5 digit or 5+4 format")
                                                                             (a ((href "/your-settings")) "Try again"))))
                                                (begin (query-exec db-conn
                                                                   "UPDATE Address
                                                                   SET address1=?,address2=?address3=?,
                                                                       city=?,state=?,zip=?
                                                                   WHERE id=?"
                                                                   address_1 address_2 address_3
                                                                   city state zip
                                                                   (vector-ref address 0))
                                                       (redirect-to "/your-settings"))))))))
                             #f))
           (bill-formlet (if user-data
                             (let ((address (if (not (sql-null? (vector-ref user-data 6)))
                                                (query-row db-conn "SELECT * FROM Address WHERE id=?" (vector-ref user-data 6))
                                                (vector 0 (format "User ~a Addr" (vector-ref user-data 1)) "" "" "" "" "" ""))))
                               (formlet
                                (div
                                 (h2 "Billing Address")
                                 "Same as shipping address?" ,[(default #"0" (checkbox #"1"
                                                                                       (sql-null? (vector-ref user-data 6)))) . => . same?]
                                 (br)
                                 "Address:    " ,[(to-string (required (text-input #:max-length 128 #:value (vector-ref address 2)))) . => . address_1]
                                 (br)
                                 "Address 2:  " ,[(to-string (required (text-input #:max-length 128 #:value (vector-ref address 3)))) . => . address_2]
                                 (br)
                                 "Address 3:  " ,[(to-string (required (text-input #:max-length 128 #:value (vector-ref address 4)))) . => . address_3]
                                 (br)
                                 "City:       " ,[(to-string (required (text-input #:max-length 128 #:value (vector-ref address 5)))) . => . city]
                                 "State:      " ,[(to-string (required (text-input #:max-length 2   #:size 2 #:value (vector-ref address 6)))) . => . state]
                                 "Zip:        " ,[(to-string (required (text-input #:max-length 10  #:size 5 #:value (vector-ref address 7)))) . => . zip]
                                 )
                            (if (equal? same? #"1") ; billing address is same as shipping address, when present delete existing billing address
                                (begin
                                  (when (not (sql-null? (vector-ref user-data 6)))
                                    (call-with-transaction db-conn
                                      (lambda ()
                                        (begin
                                          (query-exec db-conn "UPDATE Users SET Billing_Address=NULL WHERE id=?" user-id)
                                          (query-exec db-conn "DELETE FROM Address WHERE id=?" (vector-ref user-data 6))))))
                                  (redirect-to "/your-settings"))
                                (if (equal? address_1 "")
                                    (response/xexpr `(html (head (title "Missing Address"))
                                                           (body (h1 "You left \"Address\" blank")
                                                                 (a ((href "/your-settings")) "Try again"))))
                                    (if (equal? city "")
                                        (response/xexpr `(html (head (title "Missing City"))
                                                               (body (h1 "You left \"City\" blank")
                                                                     (a ((href "/your-settings")) "Try again"))))
                                        (if (not (set-member? state-abbreviations
                                                              (list->string
                                                               (map char-upcase (string->list state)))))
                                            (response/xexpr `(html (head (title "Not a valid state"))
                                                                   (body (h1 "Not a valid state abbreviation")
                                                                         (a ((href "/your-settings")) "Try again"))))
                                            (if (not (or (regexp-match #rx"....." zip)
                                                         (regexp-match #rx".....-...." zip)))
                                                (response/xexpr `(html (head (title "Bad zip format"))
                                                                       (body (h1 "Zip code not in 5 digit or 5+4 format")
                                                                             (a ((href "/your-settings")) "Try again"))))
                                                (begin 
                                                  (if (sql-null? (vector-ref user-data 6))
                                                      (call-with-transaction
                                                       db-conn
                                                       (lambda ()
                                                         (begin (query-exec db-conn
                                                                            "INSERT INTO Address VALUES
                                                                             (DEFAULT,concat_ws(' ',
                                                                                                'User',
                                                                                                (SELECT user_name
                                                                                                   FROM Users
                                                                                                  WHERE id=?),
                                                                                                'Bill Addr')
                                                                                                            ,?,?,?,?,?,?)"
                                                                            user-id
                                                                            address_1 address_2 address_3 city state zip)
                                                                (query-exec db-conn
                                                                            "UPDATE Users SET billing_address=LAST_INSERT_ID() WHERE id=?"
                                                                            user-id))))
                                                      (query-exec db-conn "UPDATE Address
                                                                           SET address1=?,address2=?address3=?,
                                                                               city=?,state=?,zip=?
                                                                           WHERE id=?"
                                                                  address_1 address_2 address_3
                                                                  city state zip
                                                                  (vector-ref address 0)))
                                                  (redirect-to "/your-settings")))))))))
                             #f))
           (schedule-formlet (if user-data
                                 (let ((shipping-schedule-table (map vector->list
                                                                    (query-rows db-conn
                                                                                "SELECT id,name FROM Release_Schedule
                                                                                 ORDER BY CASE id WHEN ? THEN 0 ELSE 1 END, name"
                                                                                 (vector-ref user-data 7)))))
                                   (formlet
                                    (div
                                     (h2 "Shipping Schedule")
                                     ,[(select-input (map first shipping-schedule-table)
                                                     #:display (lambda (key)
                                                                 (second (assoc key shipping-schedule-table))))
                                      . => . shipping-schedule])
                                    (begin (query-exec db-conn "UPDATE Users SET shipping_schedule=? WHERE id=?" shipping-schedule user-id)
                                           (redirect-to "/your-settings"))))
                                 #f))
           (shipclass-formlet (if user-data
                                  (let ((shipping-class-table (map vector->list
                                                                   (query-rows db-conn
                                                                               "SELECT id,name FROM Shipping_Class
                                                                                ORDER BY CASE id WHEN ? THEN 0 ELSE 1 END, name"
                                                                               (vector-ref user-data 8)))))
                                    (formlet
                                      (div
                                       (h2 "Shipping Class")
                                       (a ((href "/shipping-rates/2")) "See shipping rates")
                                       (br)
                                       ,[(select-input (map first shipping-class-table)
                                                       #:display (lambda (key)
                                                                   (second (assoc key shipping-class-table))))
                                         . => . shipping-class])
                                      (begin (query-exec db-conn "UPDATE Users SET shipping_class=? WHERE id=?" shipping-class user-id)
                                             (redirect-to "/your-settings"))))
                                   #f))
           )
    (if (not user-id)
        (redirect-to "/")
        (page
         (response/xexpr
          `(html (head (title "Your Settings"))
                 (body ((bgcolor "lightyellow") (text"orange"))
                       (h1 "Edit Your Settings")
                       (a ((href "/")) "Return to main")
                       ,@(map (lambda (formlet)
                               `(form ([action ,(embed/url
                                        (lambda (inner-req)
                                          (formlet-process formlet inner-req)))])
                                     ,@(formlet-display formlet)
                                     (input ([type "submit"] [value "Submit"]))))
                             (list phone-formlet email-formlet passw-formlet
                                   addr-formlet  bill-formlet  schedule-formlet
                                   shipclass-formlet))
                       )
                 ,my-footer
                 ))))))

(define (your-sub-page req)
  (letrec ((user-id (request-id-cookie "logged-in"
                                       secret-salt
                                       req))
           (user-data (if user-id
                          (query-row db-conn "SELECT * FROM Users WHERE id=?" user-id)
                          #f))
           (num-subs  (if user-id
                          (query-value db-conn "SELECT COUNT(series_id)
                                                FROM Pull_List
                                                WHERE cart=False AND user_id=?" user-id)
                          #f)))
    (if (not user-id)
        (redirect-to "/")
        (response/xexpr
         `(html
           (head (title "Your Subscriptions"))
           (body ((bgcolor "lightyellow") (text "orange"))
                 (a ((href ,"/")) "Return to main page")
                 (h1 "Your Status")
                 (table (tr (td (h2 ,(vector-ref user-data 5) " " ,(vector-ref user-data 6))))
                        (tr (td ,(format "Number of subscriptions ~a" num-subs)))
                        (tr (td ,(format "Current Balance ~a" (double->dollar (vector-ref user-data 12)))))
                        (tr (td ,(let ((date (vector-ref user-data 15)))
                                    (if (sql-null? date)
                                        "No shipments yet"
                                        (sql-date-output date)))))
                         )
                 ,(if (num-subs . > . 0)
                      `(div
                        (h1 "Your Subscriptions")
                        ,(generic-report-table '("Series", "Your Price", "Date Added", "Remaining", "Unsub")
                                               (query-rows db-conn "SELECT name,rate_at_sub,date_added,issues_left,series_id
                                                     FROM Pull_List,Web_Subscribable
                                                     WHERE user_id=? AND cart=FALSE AND id=series_id
                                                     ORDER BY name"
                                                           user-id)
                                               (lambda (vec)
                                                 (begin (vector-set! vec 1 (format "~a/issue" (double->dollar (vector-ref vec 1))))
                                                        (vector-set! vec 2 (sql-date-output (vector-ref vec 2)))
                                                        (when (sql-null? (vector-ref vec 3))
                                                          (vector-set! vec 3 "Ongoing"))
                                                        (vector-set! vec 4 `(a ((href ,(format "/unsub/~a" (vector-ref vec 4)))) "Unsub"))
                                                        vec)))
                        )
                      `(h1 "No Subscriptions yet"))
                 )
           ,my-footer
           )))))

(define (unsub-confirm req series-id)
  (let ((user-id (request-id-cookie "logged-in"
                                    secret-salt
                                    req)))
    (if (not user-id)
        (redirect-to "/")
        (page
         (response/xexpr
          `(html
            (form ([action ,(embed/url
                             (lambda (inner-req)
                               (begin (query-exec db-conn
                                                  "DELETE FROM Pull_List
                                                   WHERE  series_id=? AND user_id=?
                                                     AND  cart=False"
                                                  series-id user-id)
                                      (redirect-to "/your-subs"))))])
                  (input ([type "submit"] [value "Confirm"])))
            (form ([action ,(embed/url (lambda (inner-req)
                                         (redirect-to "/your-subs")))])
                  (input ([type "submit"] [value "Cancel"])))
            ))))))

(define (password-reset-page req [str ""])
  (let ((html-block (lambda (formlet label)
                      (page
                       (response/xexpr
                        `(html (head ((title "Password Reset")))
                               (body ((bgcolor "lightyellow") (text "orange"))
                                     (a ((href ,"/")) "Return to main page")
                                     (h1 "Password Recovery")
                                     (form [(action ,(embed/url
                                                      (lambda (inner-req)
                                                        (formlet-process formlet inner-req))))]
                                           ,@(formlet-display formlet)
                                           (input ([type "submit"] [value ,label])))
                                     )
                               ,my-footer))))))
    (if (equal? str "")
      (html-block
       (formlet
        (div
         (h2 "Email")
         ,[(to-string (required (text-input #:value "" #:max-length 128))) . => . email])
        (letrec ((user-id (query-maybe-value db-conn "SELECT DES_ENCRYPT(id,'reset')
                                                      FROM Users WHERE email=?" email)))
          (response/xexpr
           `(html (head ((title "Check your email")))
                  (body (h2 "A reset link has been sent to the provided email")
                        (p "if you entered a valid address the link will direct you to the password reset prompt")
                        (a ((href "/")) "Return to main")
                        (br)
                        (h1 "--------------------------------------------------------")
                        (h1 "Email functionality disabled, this page is a placeholder")
                        (h1 "--------------------------------------------------------")
                        ,(if (not (equal? user-id #f))
                             `(a ((href ,(format "http://~a/password-reset/~a" domain-name (bytes->hex-string user-id))))
                                 "Click here to test functionality!")
                             `(div
                               (p "An attempt was made to reset the password for an account on CosmonautKittensComics.com using this email.
                                   However there is no account associated with this email.  If you believe this to be fradulent activity, or
                                   have used this email for a account previously then please contact our system administrator with the following link")
                               (a ((href ,(format "http://~a/contact" domain-name))) "Contact Us")))
                        )
                  ))))
       "Send Link")
      (if (not (equal? str "reply"))
          (letrec ((user-id (string->number
                         (bytes->string/locale
                                         (query-value db-conn "SELECT IFNULL(DES_DECRYPT(?,'reset'),'-1')" (hex-string->bytes str)))))
               (valid-user? (query-maybe-value db-conn "SELECT user_name FROM Users WHERE id=?" user-id))
               (redir-addr (format "/password-reset/~a" str)))
        (if (not valid-user?)
            (redirect-to "/")
            (html-block
             (formlet
              (div
               (h2 "New Password")
               "password" ,[(to-string (required (password-input))) . => . password_1]
               (br)
               "confirm"  ,[(to-string (required (password-input))) . => . password_2])
              (if (equal? password_1 "")
                  (response/xexpr `(html (head (title "Blank password"))
                                         (body (h1 "Non-blank password required!")
                                               (a ((href ,redir-addr)) "Try again"))))
                  (if (not (equal? password_1 password_2))
                      (response/xexpr `(html (head (title "Password Mismatch"))
                                             (body (h1 "Password Not confirmed")
                                                   (a ((href ,redir-addr)) "Try again"))))
                      (begin (query-exec db-conn
                                         "UPDATE Users
                                             SET password=?
                                           WHERE id=?"
                                         (sha1-hash/string password_1) user-id)
                             (redirect-to "/password-reset/reply")
                             ))))
             "Submit Password")
            ))
      (response/xexpr
       `(html (head ((title "New password Set")))
              (body (h2 "Your password has been reset")
                    (p "Please make sure to memorize it or save a written copy in a secure location")
                    (a ((href "/")) "Return to main page")
                    )))))))

; a form for sending email may be added and thus a continutation may be needed
; recall that page abstracts away the embed-url and send/suspend/dispatch idiom
(define (contact-page req)
  (page
   (response/xexpr
    `(html
      (head (title "Contact Us"))
      (body ((bgcolor "lightyellow") (text "orange"))
       (p "Insert contact info here")
       (a ((href ,"/")) "Return to main page"))
      ,my-footer
       ))))

(define (logout-handler req)
  (redirect-to "/"
               #:headers (list (cookie->header (logout-id-cookie "logged-in")))))

(define (login-page req)
  (define login-formlet
    (formlet
     (div "Username:" ,[(to-string (required (text-input))) . => . user] (br)
          "Password:" ,[(to-string (required (password-input))) . => . password])
     (let ((user-data (query-maybe-row db-conn
                                  "select id,password,failed_logins from Users where user_name=? and failed_logins<?"
                                  user
                                  max-failed-logins)))
       (if (not user-data) ; user not found, we will provide no feedback on whether user was correct (no hackers guessing)
           (redirect-to "/log-in")
           (letrec ([match (equal? (sha1-hash/string password) (vector-ref user-data 1))]
                    [num (if match 0 (+ 1 (vector-ref user-data 2)))]) ; if brute force, then after repeated failed attempts
                                                                       ; with proper user name send email
                    (begin (query-exec db-conn
                                  "update Users set failed_logins=? where id=?"
                                  num
                                  (vector-ref user-data 0))
                      (if (not match)
                          (redirect-to "/log-in")
                          (redirect-to "/"
                                       see-other
                                       #:headers (list (cookie->header (make-id-cookie
                                                                  "logged-in"
                                                                  secret-salt
                                                                  (number->string (vector-ref user-data 0))))))
                          )))))))
  (page
   (response/xexpr
    `(html (head (title "Log-in to your account"))
           (body
            (h2 "Log-in to your account")
            (p "New user? " (a ((href "new-user")) "click-here") " to register" (br)
               "Forgot password? " (a ((href "password-reset")) " reset it here"))
            (form ([action ,(embed/url (lambda (inner-req)
                                         (formlet-process login-formlet inner-req)))])
                  ,@(formlet-display login-formlet)
                  (input ([type "submit"] [value "Log-in"]))))
           ,my-footer
           ))))

(define (shipping-rates-page req [src 1])
  (response/xexpr
   `(html (head (title "Shipping Rates"))
          (body ((bgcolor "lightyellow") (text "orange"))
                ,(if (= src 1)
                     `(a ((href "/")) "Return to main page")
                     `(a ((href "/your-settings")) "Return to settings"))
                (p "Below are the available shipping rates for your shipments.
                    Flat-rate packages charge a single rate no matter how many comics are being shipped.
                    Other plans are stepwise, if the first category is 10 comics then that is the rate for 1 through 10 in a single shipment.
                    If the next category for a plan is 20 then that price applies for 11 through 20 comics, and so on.")
                ,(generic-report-table '("Name" "Number" "Rate")
                                       (query-rows db-conn "SELECT name,num,CASE WHEN flat=false THEN rate ELSE '--' END AS tru_rate
                                                            FROM Shipping_Class NATURAL JOIN Shipping_Class_Rates")
                                       (let ((classes (mutable-set)))
                                         (lambda (vec)
                                               (begin (if (not (set-member? classes (vector-ref vec 0)))
                                                          (set-add! classes (vector-ref vec 0))
                                                          (vector-set! vec 0 ""))
                                                      (when (not (string? (vector-ref vec 2)))
                                                        (vector-set! vec 2 (double->dollar (vector-ref vec 2))))
                                                      vec))))
                )
          ,my-footer)))


;;;;; BEGIN ADMIN SECTION ;;;;;
(define (admin-tools-page req)
  (letrec ((user-id (request-id-cookie "logged-in"
                                       secret-salt
                                       req))
           (admin? (if user-id
                       (= 1 (query-value db-conn
                                         "SELECT admin FROM
                                          Users WHERE id=?" user-id))
                       #f)))
                       
    (response/xexpr
     (if (not admin?)
         `(html (head (title "Employees Only!"))
                (body (h1 "Employees Only!")
                      (a ((href "/")) "Return to main")))
         '(html (head (title "Admin Control Panel"))
                (body (h1 "Admin Control Panel") (br)
                      (a ((href "/")) "Return to main") (br)
                      (a ((href "/min-order-report")) "Minimum Order Report") (br)
                      (a ((href "/admin-comics-report")) "Manage Comics") (br) ; id links to edit-series, name links to admin-subscription-report
                      (a ((href "/admin-users")) "Manage Users") (br) ; id links to that users pull-list report, add an edit column (id as first and last attribute queried for
                      (a ((href "/admin-phys-users")) "Manage Physical Users") (br) ; id links to "Manage Phys. Pull-List"
                      (a ((href "/admin-publishers")) "Manage Publishers") (br) ; id links to edit-publisher
                      ))
         ))))

(define (min-order-page req)
    (letrec ((user-id (request-id-cookie "logged-in"
                                       secret-salt
                                       req))
           (admin? (if user-id
                       (= 1 (query-value db-conn
                                     "SELECT admin FROM
                                      Users WHERE id=?" user-id))
                       #f)))
      (response/xexpr
       (if (not admin?)
           `(html (head (title "Employees Only!"))
                (body (h1 "Employees Only!")
                      (a ((href "/")) "Return to main")))
           `(html (head (title "Minimum Order Report"))
                  (body (h1 "Minimum Order Report")
                        (a ((href "/admin-tools")) "Return to Admin Tools")
                        ,(generic-report-table `("ID" "Name" "Volume" "Publisher" "Number")
                           (query-rows db-conn "SELECT series_id,all_subs.name,volume,Publisher.name,COUNT(user_id)
                                                FROM   ((SELECT series_id,name,volume,publisher,user_id
                                                        FROM   Pull_List,Web_Subscribable
                                                        WHERE  id=series_id AND cart=False)
                                                       UNION
                                                       (SELECT series_id,name,volume,publisher,user_id
                                                        FROM   Physical_Pull_List,Series
                                                        WHERE  id=series_id
                                                       )) all_subs,
                                                       Publisher
                                                WHERE publisher=Publisher.id
                                                GROUP BY series_id"))))
       ))))

(define (admin-comics-page req)
      (letrec ((user-id (request-id-cookie "logged-in"
                                       secret-salt
                                       req))
               (admin? (if user-id
                           (= 1 (query-value db-conn
                                         "SELECT admin FROM
                                          Users WHERE id=?" user-id))
                       #f)))
      (page
       (response/xexpr
       (if (not admin?)
           `(html (head (title "Employees Only!"))
                (body (h1 "Employees Only!")
                      (a ((href "/")) "Return to main")))
           `(html (head (title "Manage Comics"))
                  (body (h1 "Manage Comics")
                        "Click on an ID to edit that comic's details, clock on a name to view a report of all subscribers"
                        (br)
                        (a ((href "/admin-tools")) "Return to Admin Tools")
                        (form ([action ,(embed/url (lambda (inner-req)
                                                        (redirect-to "/admin-edit-series/-1")))])
                                 (input ([type "submit"] [value "New Comic"])))
                        ,(generic-report-table `("ID" "Name" "Volume" "Publisher"
                                                 "Rating" "Start" "End" "Lim"
                                                 "Cap" "On Web?" "In Shop"
                                                 "Ongoing Rate" "Six Issue Rate"
                                                 "Release Schedule" "Delete")
                            (query-rows db-conn "SELECT SFull.id,SFull.name,volume,Publisher.name,rating,start_date,end_date,
                                                        limited_series,cap,web,shop,ongoing_rate,six_issue_rate,Release_Schedule.name,
                                                        SFull.id
                                                 FROM   (SELECT * FROM Series NATURAL LEFT OUTER JOIN Subscribable_Series) AS SFull,
                                                        Publisher,
                                                        Release_Schedule
                                                 WHERE  Publisher.id=SFull.publisher
                                                   AND  Release_Schedule.id=SFull.release_schedule
                                                 ORDER BY SFull.name")
                            (lambda (vec)
                              (let ((series-id (vector-ref vec 0))
                                    (series-name (vector-ref vec 1)))
                                (begin (vector-set! vec 5 (if (sql-null? (vector-ref vec 5))
                                                              "--"
                                                              (sql-date-output (vector-ref vec 5))))
                                       (vector-set! vec 6 (if (sql-null? (vector-ref vec 6))
                                                              "--"
                                                              (sql-date-output (vector-ref vec 6))))
                                       (vector-set! vec 7 (if (= 1 (vector-ref vec 7))
                                                              "Yes"
                                                              ""))
                                       (vector-set! vec 9 (if (= 1 (vector-ref vec 9))
                                                              "✓"
                                                              "✗"))
                                       (vector-set! vec 10 (if (= 1 (vector-ref vec 10))
                                                               "✓"
                                                               "✗"))
                                       (vector-set! vec 11 (double->dollar (vector-ref vec 11)))
                                       (vector-set! vec 12 (double->dollar (vector-ref vec 12)))
                                       (vector-set! vec 0 `(a ((href ,(format "admin-edit-series/~a"
                                                                              series-id)))
                                                              ,(~a series-id)))
                                       (vector-set! vec 1 `(a ((href ,(format "admin-subscription-report/~a"
                                                                              series-id)))
                                                              ,series-name))
                                       (vector-set! vec 14 `(a ((href ,(format "admin-comic-delete/~a"
                                                                              series-id)))
                                                              "Delete?"))
                                       vec))))
                  ))
           )))))

(define (admin-subscription-page req series-id)
  (letrec ((user-id (request-id-cookie "logged-in"
                                       secret-salt
                                       req))
           (admin? (if user-id
                       (= 1 (query-value db-conn
                                         "SELECT admin FROM
                                          Users WHERE id=?" user-id))
                       #f))
           (series-name (if admin?
                            (query-maybe-value db-conn
                                               "SELECT name FROM Series
                                                WHERE id=?" series-id)
                            #f))
           (online-rows (if admin?
                            (query-rows db-conn
                                        "SELECT user_id,user_name,first_name,last_name,
                                                rate_at_sub,issues_left,date_added
                                         FROM  Pull_List,Users
                                         WHERE user_id=Users.id
                                           AND series_id=?"
                                        series-id)
                            '()))
           (physical-rows (if admin?
                              (query-rows db-conn
                                          "SELECT user_id,first_name,last_name
                                             FROM Physical_Pull_List,Physical_Users
                                            WHERE user_id=Physical_Users.id
                                              AND series_id=?"
                                          series-id)
                              '())))
    (response/xexpr
     (if (not admin?)
         `(html (head (title "Employees Only!"))
                (body (h1 "Employees Only!")
                      (a ((href "/")) "Return to main")))
         (if (not series-name)
             `(html (head (title "Invalid id")
                          (body (h1 "Invalid id")
                                (a ((href "/admin-comics-report"))
                                   "Return to admin-comics-report"))))
             `(html (head (title ,(format "Subscription Report for ~a" series-name)))
                    (body (h1 ,series-name)
                          (a ((href "/admin-comics-report"))
                             "Return to admin-comics-report")
                          (br)
                          (a ((href "/admin-tools"))
                             "Return to admin-tools")
                          (h2 "Online")
                          ,(if (not (empty? online-rows))
                               (generic-report-table '("User ID" "User Name" "First Name" "Last Name"
                                                       "User Rate" "Issues Left" "Date Added")
                                                     online-rows
                                                     (lambda (vec)
                                                       (begin (vector-set! vec 4 (double->dollar (vector-ref vec 4)))
                                                              (when (sql-null? (vector-ref vec 5))
                                                                (vector-set! vec 5 "Ongoing"))
                                                              (vector-set! vec 6 (sql-date-output (vector-ref vec 6)))
                                                              vec)))
                               `(div (br) (h3 "None")))
                          (h2 "Physical")
                          ,(if (not (empty? physical-rows))
                               (generic-report-table '("User Id" "First Name" "Last Name")
                                                     physical-rows)
                               `(div (br) (h3 "None")))
                          )
                    ))))))

(define (admin-edit-series-page req series-id)
  (letrec ((user-id (request-id-cookie "logged-in"
                                       secret-salt
                                       req))
               (admin? (if user-id
                           (= 1 (query-value db-conn
                                         "SELECT admin FROM
                                          Users WHERE id=?" user-id))
                       #f))
               (comic-data (if admin?
                                (if (= -1 series-id)
                                    (vector "New Comic" 1)
                                    (query-maybe-row db-conn
                                               "SELECT name,volume
                                                FROM Series
                                                WHERE id=?" series-id))
                            #f))
               (series-name (if comic-data (vector-ref comic-data 0) #f))
               (series-vol  (if comic-data (vector-ref comic-data 1) #f))
               (cover-upload (if (not admin?)
                                 #f
                                 (formlet
                                  (div  
                                   "New Cover:" ,[(file-upload) . => . cover]
                                  (img ((src ,(if (file-exists? (get-image-path series-name series-vol "png"))
                                        (get-image-path series-name series-vol "png" #f)
                                        "/data/images/placeholder2.png")))))
                                  (begin (display-to-file
                                          (binding:file-content cover)
                                          (get-image-path series-name series-vol "temp")
                                          #:mode 'binary
                                          #:exists 'replace)
                                         (system/exit-code
                                                         (format "~a\\convert.exe '~a' '~a'"
                                                         cygwin-bin-path
                                                         (string-replace (get-image-path series-name series-vol "temp" #t) "/" "\\")
                                                         (string-replace (get-image-path series-name series-vol "png"  #t) "/" "\\")))
                                         (system/exit-code
                                                         (format "~a\\convert.exe '~a' -thumbnail 84x150 '~a'"
                                                         cygwin-bin-path
                                                         (string-replace (get-image-path series-name series-vol "png"  #t) "/" "\\")
                                                         (string-replace (string-replace (get-image-path series-name series-vol "png"  #t)
                                                                                         "/" "\\") ".png" "_thumb.png")))
                                         (system/exit-code
                                                         (format "~a\\rm.exe '~a'"
                                                         cygwin-bin-path
                                                         (string-replace (get-image-path series-name series-vol "temp" #t) "/" "\\")))
                                         (redirect-to (format "/admin-edit-series/~a" series-id)))
                                  )))
               (edit-formlet (if (not admin?)
                                 #f
                                 (formlet*
                                  `(div
                                    ,(generic-report-table
                                      `("ID" "Name" "Volume" "Publisher"
                                        "Rating" "Start" "End" "Lim"
                                        "Cap" "On Web?" "In Shop"
                                        "Ongoing Rate" "Six Issue Rate"
                                        "Release Schedule")
                                      (if (not (= series-id -1))
                                          (query-rows db-conn
                                                      "SELECT id,name,volume,publisher,rating,start_date,end_date,
                                                              limited_series,cap,web,shop,ongoing_rate,six_issue_rate,release_schedule
                                                       FROM   Series NATURAL JOIN Subscribable_Series
                                                       WHERE  id=?"
                                                      series-id)
                                          (list (vector series-id series-name 1 0 "N/A" sql-null sql-null 0 3 1 1 2.85 2.99 1)))
                                      (lambda (vec)
                                        (let ((publisher-table (map vector->list
                                                                    (query-rows db-conn
                                                                                "SELECT id,name FROM Publisher
                                                                                 ORDER BY CASE id WHEN ? THEN 0 ELSE 1 END, name"
                                                                                (vector-ref vec 3))))
                                              (release-schedule-table (map vector->list
                                                                           (query-rows db-conn
                                                                                       "SELECT id,name FROM Release_Schedule
                                                                                        ORDER BY CASE id WHEN ? THEN 0 ELSE 1 END, name"
                                                                                       (vector-ref vec 13))))
                                              (ratings (map (lambda (row) (vector-ref row 0))
                                                            (query-rows db-conn "(SELECT ?)
                                                                                  UNION
                                                                                 (SELECT rating FROM Rating)"
                                                                        (vector-ref vec 4)))))
                                          (begin (vector-set! vec 0 (if (= series-id -1)
                                                                        "New"
                                                                        series-id))
                                                 (vector-set! vec 1 `(div ,[(to-string (required (text-input #:value (vector-ref vec 1))))
                                                                            . =>* . name]))
                                                 (vector-set! vec 2 `(div ,[(to-string (required (text-input #:value (number->string (vector-ref vec 2)))))
                                                                            . =>* . vol]))
                                                 ; to-number doesn't work, even after to-string it fails on string?/c
                                                 (vector-set! vec 3 `(div ,[(select-input (map first publisher-table)
                                                                                          #:display (lambda (key)
                                                                                                      (second (assoc key publisher-table))))
                                                                            . =>* . publisher]))
                                                 ; to-string returns nothing and needs byte encoding, instead do nothing
                                                 (vector-set! vec 4 `(div ,[(select-input ratings)
                                                                            . =>* . rating]))
                                                 (vector-set! vec 5 `(div ,((date-select-formlet (vector-ref vec 5)). =>* . start-date)))
                                                 (vector-set! vec 6 `(div ,((date-select-formlet (vector-ref vec 6)) . =>* .   end-date)))
                                                 (vector-set! vec 7 `(div ,[(default #"0" (checkbox #"1" (= 1 (vector-ref vec 7))))
                                                                            . =>* . limited?]))
                                                 ; checkbox returns value #"1" if checked and #f otherwise, use with default
                                                 ; also to-boolean returns #f (no bytes equal #t or #f) return 0 or 1 and translate
                                                 (vector-set! vec 8 `(div ,[(to-string (required (text-input #:value (number->string (vector-ref vec 8))
                                                                                                             #:max-length 3 ; max input size
                                                                                                             #:size 3))) ; max horizontal size
                                                                            . =>* . cap]))
                                                 (vector-set! vec 9 `(div ,[(default #"0" (checkbox #"1" (= 1 (vector-ref vec 9))))
                                                                            . =>* . web?]))
                                                 (vector-set! vec 10 `(div ,[(default #"0" (checkbox #"1" (= 1 (vector-ref vec 10))))
                                                                             . =>* . shop?]))
                                                 (vector-set! vec 11 `(div ,[(to-string (required (text-input #:value (number->string (vector-ref vec 11))
                                                                                                              #:size 9)))
                                                                             . =>* . ongoing-rate]))
                                                 (vector-set! vec 12 `(div ,[(to-string (required (text-input #:value (number->string (vector-ref vec 12))
                                                                                                              #:size 9)))
                                                                             . =>* . six-issue-rate]))
                                                 (vector-set! vec 13 `(div ,[(select-input (map first release-schedule-table)
                                                                                           #:display (lambda (key)
                                                                                                       (second (assoc key release-schedule-table))))
                                                                             . =>* . release-schedule]))
                                                 vec)))))
                                    (call-with-transaction db-conn
                                     (lambda ()
                                      (begin ;(display (list name vol publisher rating limited? cap web? shop? ongoing-rate six-issue-rate release-schedule))
                                           (if (= series-id -1)
                                               (begin ; insert new tuples
                                                 (query-exec db-conn "INSERT INTO Series VALUES
                                                                     (DEFAULT,?,?,?,?,?,?,?)"
                                                             (first name) (string->number (first vol)) (first publisher)
                                                             (first rating) (bl->sql-bool limited?))
                                                 (query-exec db-conn "INSERT INTO Subscribable_Series VALUES
                                                                      (LAST_INSERT_ID(),?,?,?,?,?,?)"
                                                             (string->number (first cap)) (bl->sql-bool web?) (bl->sql-bool shop?)
                                                             (string->number (first ongoing-rate))
                                                             (string->number (first six-issue-rate))
                                                             (first release-schedule)
                                                             (if (equal? (first start-date) "")
                                                                 sql-null
                                                                 (selector-date->sql-date (first start-date)))
                                                             (if (equal? (first end-date) "")
                                                                 sql-null
                                                                 (selector-date->sql-date (first end-date))))
                                                 )
                                               (begin ; update existing tuples
                                                 (query-exec db-conn "UPDATE Series
                                                                         SET name=?,volume=?,publisher=?,rating=?,
                                                                             limited_series=?,start_date=?,end_date=?
                                                                       WHERE id=?"
                                                             (first name) (string->number (first vol)) (first publisher)
                                                             (first rating) (bl->sql-bool limited?)
                                                             (if (equal? (first start-date) "")
                                                                 sql-null
                                                                 (selector-date->sql-date (first start-date)))
                                                             (if (equal? (first end-date) "")
                                                                 sql-null
                                                                 (selector-date->sql-date (first end-date)))
                                                             series-id)
                                                 (query-exec db-conn "UPDATE Subscribable_Series
                                                                         SET cap=?,web=?,shop=?,ongoing_rate=?,six_issue_rate=?,release_schedule=?
                                                                       WHERE id=?"
                                                             (string->number (first cap)) (bl->sql-bool web?) (bl->sql-bool shop?)
                                                             (string->number (first ongoing-rate))
                                                             (string->number (first six-issue-rate))
                                                             (first release-schedule)
                                                             series-id)
                                                 ))
                                           (if (not (= series-id -1))
                                               (redirect-to (format "/admin-edit-series/~a" series-id))
                                               (redirect-to "/admin-comics-report")))))
                                    ))))
    (page
     (response/xexpr
      (if (not admin?)
          `(html (head (title "Employees Only!"))
                 (body (h1 "Employees Only!")
                       (a ((href "/")) "Return to main")))
          (if (not series-name)
              `(html (head (title "Invalid Series"))
                     (body (h1 "Invalid Series")
                           (a ((href "/admin-comics-report"))
                              "Return to admin-comics-report")))
              `(html (head (title ,(format "Edit ~a Details" series-name)))
                     (body (h1 ,series-name)
                           (a ((href "/admin-comics-report"))
                              "Return to admin-comics-report")
                           (br)
                           (a ((href "/admin-tools"))
                              "Return to admin-tools")
                           (form ([action ,(embed/url (lambda (inner-req)
                                                        (formlet-process edit-formlet inner-req)))])
                                 ,@(formlet-display edit-formlet)
                                 (input ([type "submit"] [value "Submit Updates"])))
                           (form ([action ,(embed/url (lambda (inner-req)
                                                        (redirect-to (format "/admin-edit-series/~a" series-id))))])
                                 (input ([type "submit"] [value "Reset"])))
                           ,(if (not (= series-id -1))
                                `(div
                                  (form ([action ,(embed/url (lambda (inner-req)
                                                               (formlet-process cover-upload inner-req)))]
                                         [method "POST"]
                                         [enctype "multipart/form-data"])
                                        ,@(formlet-display cover-upload)
                                        (input ([type "submit"] [value "Upload cover"])))
                                  )
                                `(div))
                           ))
              ))))))

(define (admin-comic-delete req series-id)
  (letrec ((user-id (request-id-cookie "logged-in"
                                       secret-salt
                                       req))
           (admin? (if user-id
                       (= 1 (query-value db-conn
                                         "SELECT admin FROM
                                          Users WHERE id=?" user-id))
                       #f))
           (series-name (if admin?
                            (if (= -1 series-id)
                                "New Comic"
                                (query-maybe-value db-conn
                                                   "SELECT name FROM Series
                                                     WHERE id=?" series-id))
                            #f)))
           (page
            (response/xexpr
             (if (not admin?)
                 `(html (head (title "Employees Only!"))
                        (body (h1 "Employees Only!")
                              (a ((href "/")) "Return to main")))
                 (if (not series-name)
                     `(html (head (title "Invalid Series"))
                            (body (h1 "Invalid Series")
                                  (a ((href "/admin-comics-report"))
                                     "Return to admin-comics-report")))
                     `(html (head (title ,(format "Confirm ~a Deletion" series-name)))
                            (body (h1 ,series-name)
                                  (a ((href "/admin-comics-report"))
                                     "Return to admin-comics-report")
                                  (br)
                                  (a ((href "/admin-tools"))
                                     "Return to admin-tools")
                                  (h2 ,(format "Number of subscriptions to be deleted: ~a"
                                     (query-value db-conn "SELECT A.s + B.s
                                                           FROM (SELECT COUNT(user_id) AS s
                                                                 FROM Pull_List WHERE cart=False AND series_id=?) A,
                                                                (SELECT COUNT(user_id) AS s
                                                                 FROM Physical_Pull_List WHERE series_id=?) B"
                                                 series-id series-id)))
                                  (h2 ,(format "Prepay sub. refund cost: ~a"
                                     (double->dollar
                                      (query-value db-conn "SELECT COALESCE(SUM(issues_left*rate_at_sub),0)
                                                            FROM   Pull_List
                                                            WHERE  cart=False AND series_id=?" series-id))))
                                  (h2 ,(format "Ongoing revenue/issue lost: ~a"
                                     (double->dollar
                                      (query-value db-conn "SELECT COALESCE(SUM(rate_at_sub),0)
                                                            FROM   Pull_List
                                                            WHERE  issues_left IS NULL AND cart=False AND series_id=?" series-id))))
                                  (form ([action ,(embed/url (lambda (inner-req)
                                                               (call-with-transaction db-conn
                                                                (lambda ()
                                                                  (begin
                                                                    ; make the group message appear before the components, if failure then rollback will erase this
                                                                    (query-exec db-conn "INSERT INTO Web_Transactions VALUES (?,NOW(),?,0)"
                                                                                user-id (format "~a Deleted" series-name))
                                                                    ; here b/c mysql suport for cascaded trigger firing is STILL non-existant! 
                                                                    (query-exec db-conn "DELETE FROM Pull_list WHERE series_id=?" series-id)
                                                                    (query-exec db-conn "DELETE FROM Subscribable_Series WHERE id=?" series-id)
                                                                    (query-exec db-conn "DELETE FROM Series WHERE id=?" series-id)
                                                                    (redirect-to "/admin-comics-report"))))))])
                                        (input ([type "submit"] [value "Confirm"])))
                                  (form ([action ,(embed/url (lambda (inner-req)
                                                               (redirect-to "/admin-comics-report")))])
                                        (input ([type "submit"] [value "Cancel"])))
                             ))
                     ))))))

(define (admin-physical-users-page req)
  (letrec ((user-id (request-id-cookie "logged-in"
                                       secret-salt
                                       req))
           (admin? (if user-id
                       (= 1 (query-value db-conn
                                         "SELECT admin FROM
                                          Users WHERE id=?" user-id))
                       #f))
           (physical-rows (if admin?
                              (query-rows db-conn
                                          "SELECT P.id,first_name,last_name,phone,email,
                                                  concat_ws(' ',address1,address2,address3) AS st_address,
                                                  city,state,zip,
                                                  P.id,P.id,P.id
                                             FROM Physical_Users P, Address A
                                            WHERE A.id=P.address")
                              '())))
      (page
       (response/xexpr
       (if (not admin?)
           `(html (head (title "Employees Only!"))
                (body (h1 "Employees Only!")
                      (a ((href "/")) "Return to main")))
           `(html (head (title "Manage Physical Users"))
                  (body (h1 "Manage Physical Users")
                        (br)
                        (a ((href "/admin-tools")) "Return to Admin Tools")
                        (form ([action ,(embed/url (lambda (inner-req)
                                                        (redirect-to "/admin-edit-phys-user/-1")))])
                                 (input ([type "submit"] [value "New User"])))
                        ,(if (empty? physical-rows)
                             `(h2 "No Physical Users")
                             `(div ,(generic-report-table `("ID" "First Name" "Last Name" "Phone" "Email"
                                                            "Address" "City" "State" "Zip" "Subs" "Edit" "Delete?")
                                                   physical-rows
                                                   (lambda (vec)
                                                     (begin (vector-set! vec 9 `(a ((href ,(format "/admin-phys-sub-report/~a"  (vector-ref vec 9))))
                                                                                   "Report"))
                                                            (vector-set! vec 10 `(a ((href ,(format "/admin-edit-phys-user/~a"   (vector-ref vec 10))))
                                                                                   "Edit"))
                                                            (vector-set! vec 11 `(a ((href ,(format "/admin-delete-phys-user/~a" (vector-ref vec 11))))
                                                                                   "Delete"))
                                                            vec)))))
                        )
                  ))))))

(define (admin-phys-sub-report-page req arg-user-id)
  (letrec ((user-id (request-id-cookie "logged-in"
                                       secret-salt
                                       req))
           (admin? (if user-id
                       (= 1 (query-value db-conn
                                         "SELECT admin FROM
                                          Users WHERE id=?" user-id))
                       #f))
           (user-data (if admin?
                          (query-maybe-row db-conn
                                           "SELECT first_name,last_name
                                            FROM   Physical_Users
                                            WHERE  id=?" arg-user-id)
                          #f))
           (user-rows (if user-data
                              (query-rows db-conn
                                          "SELECT S.id,name,ongoing_rate
                                            FROM Series NATURAL JOIN Subscribable_Series S, Physical_Pull_List L
                                           WHERE series_id=id
                                             AND user_id=?" arg-user-id)
                          '()))
           (add-formlet (if (not admin?)
                            #f
                            (let ((comics-table (map vector->list ; assuming no user subs everything, else assoc list below is empty and errors abound 
                                                     (query-rows db-conn
                                                                 "SELECT id,name
                                                                  FROM Series NATURAL JOIN Subscribable_Series
                                                                  WHERE shop=True
                                                                    AND id NOT IN (SELECT series_id
                                                                                   FROM   Physical_Pull_LIst
                                                                                   WHERE  user_id=?)"
                                                                 arg-user-id))))
                              (formlet (div ,[(select-input (map first comics-table)
                                                            #:display (lambda (key)
                                                                        (second (assoc key comics-table))))
                                              . => . series-id])
                                       (begin (query-exec db-conn
                                                   "INSERT INTO Physical_Pull_List VALUES (?,?)"
                                                   arg-user-id series-id)
                                              (redirect-to (format "/admin-phys-sub-report/~a" arg-user-id))))
                              ))))
    (page
     (response/xexpr
      (if (not admin?)
          `(html (head (title "Employees Only!"))
                 (body (h1 "Employees Only!")
                       (a ((href "/")) "Return to main")))
          (if (not user-data)
              `(html (head (title "Invalid Phys. User"))
                     (body (h1 "Invalid Physical User")
                           (a ((href "/admin-phys-users"))
                              "Return to admin-phys-users")))
              `(html (head (title ,(vector-ref user-data 0) " " ,(vector-ref user-data 1)))
                     (body (h1 ,(vector-ref user-data 0) " " ,(vector-ref user-data 1))
                           (a ((href "/admin-phys-users"))
                              "Return to admin-phys-users")
                           (br)
                           (a ((href "/admin-tools"))
                              "Return to admin-tools")
                           ,(if (empty? user-rows)
                                `(h2 "No Subscriptions")
                                (generic-report-table
                                 '("ID" "Name" "Rate")
                                 user-rows
                                 (lambda (vec)
                                   (begin (vector-set! vec 2 (double->dollar (vector-ref vec 2)))
                                          vec))
                                 ))
                           (form ([action ,(embed/url (lambda (inner-req)
                                                        (formlet-process add-formlet inner-req)))])
                                 ,@(formlet-display add-formlet)
                                 (input ([type "submit"] [value "Add New Sub"])))
                           ))
              ))))))

(define (admin-edit-phys-user req arg-user-id)
  (letrec ((user-id (request-id-cookie "logged-in"
                                       secret-salt
                                       req))
           (admin? (if user-id
                       (= 1 (query-value db-conn
                                         "SELECT admin FROM
                                          Users WHERE id=?" user-id))
                       #f))
           (user-data (if admin?
                          (if (= -1 arg-user-id)
                              (vector "New" "User" -1)
                              (query-maybe-row db-conn
                                               "SELECT first_name,last_name,address
                                                FROM Physical_Users
                                                WHERE id=?" arg-user-id))
                          #f))
           (address-id (if user-data (vector-ref user-data 2) #f))
           (edit-formlet (if (not admin?)
                             #f
                             (formlet*
                              `(div
                                ,(generic-report-table
                                  `("ID" "First Name" "Last Name" "Phone" "email"
                                    "Address1" "Address2" "Address3" "City" "State" "Zip")
                                  (if (not (= arg-user-id -1))
                                      (query-rows db-conn
                                                  "SELECT U.id,first_name,last_name,phone,email,
                                                          address1,address2,address3,city,state,zip
                                                   FROM   Physical_Users U, Address A
                                                   WHERE  U.id=? AND U.address=A.id"
                                                  arg-user-id)
                                      (list (vector arg-user-id "" "" "" "" "" "" "" "" "" "")))
                                  (lambda (vec)
                                    (begin (vector-set! vec 0 (if (= arg-user-id -1)
                                                                  "New"
                                                                  arg-user-id))
                                           (vector-set! vec 1 `(div ,[(to-string (required (text-input #:value (vector-ref vec 1)
                                                                                                       #:max-length 128
                                                                                                       #:size 15)))
                                                                      . =>* . first-name]))
                                           (vector-set! vec 2 `(div ,[(to-string (required (text-input #:value (vector-ref vec 2)
                                                                                                       #:max-length 128
                                                                                                       #:size 15)))
                                                                      . =>* . last-name]))
                                           (vector-set! vec 3 `(div ,[(to-string (required (text-input #:value (vector-ref vec 3)
                                                                                                       #:max-length 15
                                                                                                       #:size 15)))
                                                                      . =>* . phone]))
                                           (vector-set! vec 4 `(div ,[(to-string (required (text-input #:value (vector-ref vec 4)
                                                                                                       #:max-length 128)))
                                                                      . =>* . email]))
                                           (vector-set! vec 5 `(div ,[(to-string (required (text-input #:value (vector-ref vec 5)
                                                                                                       #:max-length 128
                                                                                                       #:size 20)))
                                                                      . =>* . address1]))
                                           (vector-set! vec 6 `(div ,[(to-string (required (text-input #:value (vector-ref vec 6)
                                                                                                       #:max-length 128
                                                                                                       #:size 20)))
                                                                      . =>* . address2]))
                                           (vector-set! vec 7 `(div ,[(to-string (required (text-input #:value (vector-ref vec 7)
                                                                                                       #:max-length 128
                                                                                                       #:size 20)))
                                                                      . =>* . address3]))
                                           (vector-set! vec 8 `(div ,[(to-string (required (text-input #:value (vector-ref vec 8)
                                                                                                       #:max-length 128
                                                                                                       #:size 20)))
                                                                      . =>* . city]))
                                           (vector-set! vec 9 `(div ,[(to-string (required (text-input #:value (vector-ref vec 9)
                                                                                                       #:max-length 2
                                                                                                       #:size 2)))
                                                                      . =>* . state]))
                                           (vector-set! vec 10 `(div ,[(to-string (required (text-input #:value (vector-ref vec 10)
                                                                                                       #:max-length 10
                                                                                                       #:size 5)))
                                                                      . =>* . zip]))
                                           vec))))
                              (call-with-transaction db-conn
                               (lambda ()
                                 (begin
                                   (if (= arg-user-id -1)
                                       (begin
                                         (query-exec db-conn "INSERT INTO Address VALUES
                                                              (DEFAULT,?,?,?,?,?,?,?)"
                                                     "" ; blank name for now, update with pattern using new id code later
                                                     (first address1) (first address2) (first address3)
                                                     (first city) (first state) (first zip))
                                         (query-exec db-conn "INSERT INTO Physical_Users VALUES
                                                              (DEFAULT,?,?,?,?,(SELECT LAST_INSERT_ID()))"
                                                     (first first-name) (first last-name)
                                                     (sanitize-phone (first phone))
                                                     (first email))
                                         (query-exec db-conn "UPDATE Address
                                                              SET name=concat('Phys. User ',cast(LAST_INSERT_id() AS char(6)))
                                                              WHERE id=LAST_INSERT_ID()")
                                         )
                                       (begin
                                         (query-exec db-conn "UPDATE Address
                                                              SET address1=?,address2=?,address3=?,city=?,state=?,zip=?
                                                              WHERE id=?"
                                                     (first address1) (first address2) (first address3)
                                                     (first city) (first state) (first zip)
                                                     address-id)
                                         (query-exec db-conn "UPDATE Physical_Users
                                                              SET first_name=?,last_name=?,phone=?,email=?,address=?
                                                              WHERE id=?"
                                                     (first first-name) (first last-name)
                                                     (sanitize-phone (first phone))
                                                     (first email) (string->number (first address-id)))
                                         ))
                                   (if (not (= arg-user-id -1))
                                       (redirect-to (format "/admin-edit-phys-user/~a" arg-user-id))
                                       (redirect-to "/admin-phys-users")))))
                              ))))
    (page
     (response/xexpr
      (if (not admin?)
          `(html (head (title "Employees Only!"))
                 (body (h1 "Employees Only!")
                       (a ((href "/")) "Return to main")))
          (if (not user-data)
              `(html (head (title "Invalid Phys. User"))
                     (body (h1 "Invalid Phys. User")
                           (a ((href "/admin-phys-users"))
                              "Return to admin-phys-users")))
              `(html (head (title ,(format "Edit ~a ~a Details"
                                           (vector-ref user-data 0)
                                           (vector-ref user-data 1))))
                     (body (h1 ,(vector-ref user-data 0) " " ,(vector-ref user-data 1))
                           (a ((href "/admin-phys-users"))
                              "Return to admin-phys-users")
                           (br)
                           (a ((href "/admin-tools"))
                              "Return to admin-tools")
                           (form ([action ,(embed/url (lambda (inner-req)
                                                        (formlet-process edit-formlet inner-req)))])
                                 ,@(formlet-display edit-formlet)
                                 (input ([type "submit"] [value "Submit Updates"])))
                           (form ([action ,(embed/url (lambda (inner-req)
                                                        (redirect-to (format "/admin-edit-phys-user/~a" arg-user-id))))])
                                 (input ([type "submit"] [value "Reset"])))
                           ))
              ))))))

(define (admin-delete-phys-user-page req arg-user-id)
  (letrec ((user-id (request-id-cookie "logged-in"
                                       secret-salt
                                       req))
           (admin? (if user-id
                       (= 1 (query-value db-conn
                                         "SELECT admin FROM
                                          Users WHERE id=?" user-id))
                       #f))
           (user-data (query-maybe-row db-conn "SELECT first_name,last_name,address FROM Physical_Users WHERE id=?" arg-user-id)))
    (page
            (response/xexpr
             (if (not admin?)
                 `(html (head (title "Employees Only!"))
                        (body (h1 "Employees Only!")
                              (a ((href "/")) "Return to main")))
                 (if (not user-data)
                     `(html (head (title "Invalid Phys. User"))
                            (body (h1 "Invalid Phys. User")
                                  (a ((href "/admin-phys-users"))
                                     "Return to admin-phys-users")))
                     `(html (head (title ,(format "Confirm ~a ~a Account Deletion"
                                                  (vector-ref user-data 0)
                                                  (vector-ref user-data 1))))
                            (body (h1 ,(format "Confirm ~a ~a Account Deletion"
                                                  (vector-ref user-data 0)
                                                  (vector-ref user-data 1)))
                                  (a ((href "/admin-phys-users"))
                                     "Return to admin-phys-users")
                                  (br)
                                  (a ((href "/admin-tools"))
                                     "Return to admin-tools")
                                  (form ([action ,(embed/url (lambda (inner-req)
                                                               (call-with-transaction db-conn
                                                                (lambda ()
                                                                  (begin
                                                                    (query-exec db-conn "DELETE FROM Address WHERE id=?" (vector-ref user-data 2))
                                                                    (query-exec db-conn "DELETE FROM Physical_Users WHERE id=?" arg-user-id)
                                                                    (query-exec db-conn "INSERT INTO Web_Transactions VALUES (?,NOW(),?,0)"
                                                                                user-id (format "Phys. User ~a ~a Deleted"
                                                                                                (vector-ref user-data 0) (vector-ref user-data 1)))
                                                                    (redirect-to "/admin-phys-users"))))))])
                                        (input ([type "submit"] [value "Confirm"])))
                                  (form ([action ,(embed/url (lambda (inner-req)
                                                               (redirect-to "/admin-phys-users")))])
                                        (input ([type "submit"] [value "Cancel"])))
                                  ))
                     ))))))

(define (admin-users-page req)
  (letrec ((user-id (request-id-cookie "logged-in"
                                       secret-salt
                                       req))
           (admin? (if user-id
                       (= 1 (query-value db-conn
                                         "SELECT admin FROM
                                          Users WHERE id=?" user-id))
                       #f))
           (user-rows (if admin?
                          (query-rows db-conn
                                      "SELECT U.id,user_name,admin,first_name,last_name,
                                              concat_ws(' ',A.address1,A.address2,A.address3,A.city,A.state,A.zip) AS Shipping_Address,
                                              CASE WHEN billing_address IS NULL
                                                   THEN ''
                                                   ELSE (SELECT concat_ws(' ',B.address1,B.address2,B.address3,B.city,B.state,B.zip)
                                                         FROM Address B
                                                         WHERE B.id=U.billing_address)
                                                   END AS Billing_Address,
                                              phone,email,verified_email,balance,
                                              SS.name,SC.name,last_shipment_date
                                       FROM   Users U, Address A, Release_Schedule SS, Shipping_Class SC
                                       WHERE  A.id=address AND SS.id=shipping_schedule AND SC.id=shipping_class")
                          '())))
           (page
            (response/xexpr
             (if (not admin?)
                 `(html (head (title "Employees Only!"))
                        (body (h1 "Employees Only!")
                              (a ((href "/")) "Return to main")))
                 `(html (head (title "View Users"))
                        (body (h1 "View Users")
                        (br)
                        (a ((href "/admin-tools")) "Return to Admin Tools")
                        ,(if (empty? user-rows)
                             `(h2 "No Publishers")
                             `(div (p "Click on a Users ID to view a report of all their subscriptions")
                                    ,(generic-report-table '("ID" "User Name" "Admin?" "First Name" "Last Name" "Shipping Address" "Billing Address"
                                                            "Phone" "email" "ver?" "Balance" "Schedule" "Class" "Last Shipment")
                                                          user-rows
                                                          (lambda (vec)
                                                            (begin (vector-set! vec 0 `(a ((href ,(format "/admin-user-sub-report/~a" (vector-ref vec 0))))
                                                                                                 ,(~a (vector-ref vec 0))))
                                                                   (vector-set! vec 2 (if (= 1 (vector-ref vec 2))
                                                                                          "✓"
                                                                                          ""))
                                                                   (vector-set! vec 9 (if (= 1 (vector-ref vec 9))
                                                                                          "✓"
                                                                                          "✗"))
                                                                   (vector-set! vec 10 (double->dollar (vector-ref vec 10)))
                                                                   (when (sql-null? (vector-ref vec 13))
                                                                     (vector-set! vec 13 "--"))
                                                                   vec)))))
                        )
                 ))))))

(define (admin-user-sub-report-page req arg-user-id)
  (letrec ((user-id (request-id-cookie "logged-in"
                                       secret-salt
                                       req))
           (admin? (if user-id
                       (= 1 (query-value db-conn
                                         "SELECT admin FROM
                                          Users WHERE id=?" user-id))
                       #f))
           (user-data (if admin?
                          (query-maybe-row db-conn
                                           "SELECT first_name,last_name
                                            FROM   Users
                                            WHERE  id=?" arg-user-id)
                          #f))
           (user-rows (if user-data
                              (query-rows db-conn
                                          "SELECT S.id,name,rate_at_sub
                                            FROM Series NATURAL JOIN Subscribable_Series S, Pull_List L
                                           WHERE series_id=id
                                             AND user_id=?" arg-user-id)
                          '()))
           )
    (page
     (response/xexpr
      (if (not admin?)
          `(html (head (title "Employees Only!"))
                 (body (h1 "Employees Only!")
                       (a ((href "/")) "Return to main")))
          (if (not user-data)
              `(html (head (title "Invalid User"))
                     (body (h1 "Invalid User")
                           (a ((href "/admin-users"))
                              "Return to admin-users")))
              `(html (head (title ,(vector-ref user-data 0) " " ,(vector-ref user-data 1)))
                     (body (h1 ,(vector-ref user-data 0) " " ,(vector-ref user-data 1))
                           (a ((href "/admin-users"))
                              "Return to admin-users")
                           (br)
                           (a ((href "/admin-tools"))
                              "Return to admin-tools")
                           ,(if (empty? user-rows)
                                `(h2 "No Subscriptions")
                                (generic-report-table
                                 '("ID" "Name" "Rate")
                                 user-rows
                                 (lambda (vec)
                                   (begin (vector-set! vec 2 (double->dollar (vector-ref vec 2)))
                                          vec))
                                 ))))
              ))))))

(define (admin-publishers-page req)
  (letrec ((user-id (request-id-cookie "logged-in"
                                       secret-salt
                                       req))
           (admin? (if user-id
                       (= 1 (query-value db-conn
                                         "SELECT admin FROM
                                          Users WHERE id=?" user-id))
                       #f))
           (publisher-rows (if admin?
                               (query-rows db-conn
                                           "SELECT P.id,P.name,owner,phone,
                                                   concat_ws(' ',address1,address2,address3) AS st_address,
                                                   city,state,zip,
                                                   P.id,P.id
                                              FROM Publisher P, Address A
                                             WHERE A.id=P.address")
                               '())))
    (page
       (response/xexpr
       (if (not admin?)
           `(html (head (title "Employees Only!"))
                (body (h1 "Employees Only!")
                      (a ((href "/")) "Return to main")))
           `(html (head (title "Manage Publishers"))
                  (body (h1 "Manage Publishers")
                        (br)
                        (a ((href "/admin-tools")) "Return to Admin Tools")
                        (form ([action ,(embed/url (lambda (inner-req)
                                                        (redirect-to "/admin-edit-publisher/-1")))])
                                 (input ([type "submit"] [value "New Publisher/Imprint"])))
                        ,(if (empty? publisher-rows)
                             `(h2 "No Publishers")
                             `(div ,(generic-report-table `("ID" "Name" "Owner-ID" "Phone"
                                                            "Address" "City" "State" "Zip" "Edit" "Delete?")
                                                   publisher-rows
                                                   (lambda (vec)
                                                     (begin (when (sql-null? (vector-ref vec 2))
                                                              (vector-set! vec 2 "--"))
                                                            ;(vector-set! vec 9 `(a ((href ,(format "/admin-publisher-report/~a"  (vector-ref vec 9))))
                                                            ;                       "Report"))
                                                            (vector-set! vec 8 `(a ((href ,(format "/admin-edit-publisher/~a"   (vector-ref vec 8))))
                                                                                   "Edit"))
                                                            (vector-set! vec 9 `(a ((href ,(format "/admin-delete-publisher/~a" (vector-ref vec 9))))
                                                                                   "Delete"))
                                                            vec)))))
                        )
                  ))))))

;(define (admin-publisher-report-page req pub-id)
;  '())

(define (admin-edit-publisher-page req pub-id)
  (letrec ((user-id (request-id-cookie "logged-in"
                                       secret-salt
                                       req))
           (admin? (if user-id
                       (= 1 (query-value db-conn
                                         "SELECT admin FROM
                                          Users WHERE id=?" user-id))
                       #f))
           (publisher-data (if admin?
                               (if (= -1 pub-id)
                                   (vector "New" -1)
                                   (query-maybe-row db-conn
                                                    "SELECT name,address
                                                     FROM Publisher
                                                     WHERE id=?" pub-id))
                               #f))
           (address-id (if publisher-data (vector-ref publisher-data 1) #f))
           (edit-formlet (if (not admin?)
                             #f
                             (formlet*
                              `(div
                                ,(generic-report-table
                                  `("ID" "Name" "Owner" "Phone"
                                   "Address1" "Address2" "Address3" "City" "State" "Zip")
                                  (if (not (= pub-id -1))
                                      (query-rows db-conn
                                                  "SELECT P.id,P.name,IFNULL(owner,0),phone,
                                                          address1,address2,address3,city,state,zip
                                                   FROM   Publisher P, Address A
                                                   WHERE  P.id=? AND P.address=A.id"
                                                  pub-id)
                                      (list (vector pub-id "" 0 "" "" "" "" "" "" "")))
                                  (lambda (vec)
                                    (let ((owner-table (map vector->list
                                                            (query-rows db-conn
                                                                        "SELECT id,name
                                                                         FROM (SELECT id,name FROM Publisher
                                                                               UNION
                                                                               SELECT 0 AS id, '--' AS name) Pub
                                                                         WHERE id<>?
                                                                         ORDER BY CASE id WHEN ? THEN 0 ELSE 1 END, name"
                                                                        (vector-ref vec 0)
                                                                        (vector-ref vec 2)))))
                                      (begin (vector-set! vec 0 (if (= pub-id -1)
                                                                  "New"
                                                                  pub-id))
                                           (vector-set! vec 1 `(div ,[(to-string (required (text-input #:value (vector-ref vec 1)
                                                                                                       #:max-length 128
                                                                                                       #:size 15)))
                                                                      . =>* . name]))
                                           (vector-set! vec 2 `(div ,[(select-input (map first owner-table)
                                                                                    #:display (lambda (key)
                                                                                                (second (assoc (if (sql-null? key)
                                                                                                                   0
                                                                                                                   key)
                                                                                                               owner-table))))
                                                                      . =>* . owner-id]))
                                           (vector-set! vec 3 `(div ,[(to-string (required (text-input #:value (vector-ref vec 3)
                                                                                                       #:max-length 15
                                                                                                       #:size 15)))
                                                                      . =>* . phone]))
                                           (vector-set! vec 4 `(div ,[(to-string (required (text-input #:value (vector-ref vec 4)
                                                                                                       #:max-length 128
                                                                                                       #:size 20)))
                                                                      . =>* . address1]))
                                           (vector-set! vec 5 `(div ,[(to-string (required (text-input #:value (vector-ref vec 5)
                                                                                                       #:max-length 128
                                                                                                       #:size 20)))
                                                                      . =>* . address2]))
                                           (vector-set! vec 6 `(div ,[(to-string (required (text-input #:value (vector-ref vec 6)
                                                                                                       #:max-length 128
                                                                                                       #:size 20)))
                                                                      . =>* . address3]))
                                           (vector-set! vec 7 `(div ,[(to-string (required (text-input #:value (vector-ref vec 7)
                                                                                                       #:max-length 128
                                                                                                       #:size 20)))
                                                                      . =>* . city]))
                                           (vector-set! vec 8 `(div ,[(to-string (required (text-input #:value (vector-ref vec 8)
                                                                                                       #:max-length 2
                                                                                                       #:size 2)))
                                                                      . =>* . state]))
                                           (vector-set! vec 9 `(div ,[(to-string (required (text-input #:value (vector-ref vec 9)
                                                                                                       #:max-length 10
                                                                                                       #:size 5)))
                                                                      . =>* . zip]))
                                           vec)))))
                                (call-with-transaction db-conn
                                  (lambda ()
                                    (begin
                                      (if (= pub-id -1)
                                          (begin
                                            (query-exec db-conn "INSERT INTO Address VALUES
                                                                 (DEFAULT,?,?,?,?,?,?,?)"
                                                     (first name)
                                                     (first address1) (first address2) (first address3)
                                                     (first city) (first state) (first zip))
                                            (query-exec db-conn "INSERT INTO Publisher VALUES
                                                                 (DEFAULT,?,?,?,LAST_INSERT_ID())"
                                                     (first name) (first owner-id) (sanitize-phone (first phone)))
                                            )
                                          (begin
                                            (query-exec db-conn "UPDATE Address
                                                                 SET address1=?,address2=?,address3=?,city=?,state=?,zip=?
                                                                 WHERE id=?"
                                                     (first address1) (first address2) (first address3)
                                                     (first city) (first state) (first zip)
                                                     (first address-id))
                                            (query-exec db-conn "UPDATE Publisher
                                                                 SET name=?,owner=?,phone=?,address=?
                                                                 WHERE id=?"
                                                     (first name) (first owner-id) (sanitize-phone (first phone))
                                                     (string->number (first address-id)))
                                          ))
                                    (if (not (= pub-id -1))
                                        (redirect-to (format "/admin-edit-publisher/~a" pub-id))
                                        (redirect-to (format "/admin-publishers"))))))
                                ))))
    (page
     (response/xexpr
      (if (not admin?)
           `(html (head (title "Employees Only!"))
                (body (h1 "Employees Only!")
                      (a ((href "/")) "Return to main")))
           (if (not publisher-data)
              `(html (head (title "Invalid Publisher"))
                     (body (h1 "Invalid Publisher")
                           (a ((href "/admin-publishers"))
                              "Return to admin-publishers")))
              `(html (head (title ,(format "Edit ~a Details"
                                           (vector-ref publisher-data 0))))
                     (body (h1 ,(vector-ref publisher-data 0))
                           (a ((href "/admin-publishers"))
                              "Return to admin-publishers")
                           (br)
                           (a ((href "/admin-tools"))
                              "Return to admin-tools")
                           (form ([action ,(embed/url (lambda (inner-req)
                                                        (formlet-process edit-formlet inner-req)))])
                                 ,@(formlet-display edit-formlet)
                                 (input ([type "submit"] [value "Submit Updates"])))
                           (form ([action ,(embed/url (lambda (inner-req)
                                                        (redirect-to (format "/admin-edit-publisher/~a" pub-id))))])
                                 (input ([type "submit"] [value "Reset"])))
                           ))
              ))))))

(define (admin-delete-publisher-page req pub-id)
  (letrec ((user-id (request-id-cookie "logged-in"
                                       secret-salt
                                       req))
           (admin? (if user-id
                       (= 1 (query-value db-conn
                                         "SELECT admin FROM
                                          Users WHERE id=?" user-id))
                       #f))
           (publisher-data (if admin?
                               (query-maybe-row db-conn
                                                "SELECT name,address,IFNULL(owner,0)
                                                 FROM Publisher
                                                 WHERE id=?" pub-id)
                           #f))
           (publisher-name (if publisher-data (vector-ref publisher-data 0) #f))
           (address-id (if publisher-data (vector publisher-data 1) #f))
           (owner-id (if publisher-data (vector-ref publisher-data 2) #f))
           (transfer-formlet (if (not admin?)
                                 #f
                                 (let ((owner-table (map vector->list
                                                         (query-rows db-conn
                                                                     "SELECT id,name
                                                                      FROM (SELECT id,name FROM Publisher
                                                                            UNION
                                                                            SELECT 0 AS id, '--' AS name) Pub
                                                                      WHERE id<>?
                                                                      ORDER BY CASE id WHEN ? THEN 0 ELSE 1 END, name"
                                                                     pub-id owner-id))))
                                 (formlet `(div ,[(select-input (map first owner-table)
                                                                #:display (lambda (key)
                                                                            (second (assoc (if (sql-null? key)
                                                                                               0
                                                                                               key)
                                                                                           owner-table))))
                                                  . => . new-owner-id])
                                          (call-with-transaction db-conn
                                            (lambda ()
                                              (begin
                                                (query-exec db-conn
                                                            "INSERT INTO Web_Transactions VALUES (?,NOW(),?,0)"
                                                            user-id (if (= 0 new-owner-id)
                                                                        (format "Pub. ~a DISSOLVED" publisher-name)
                                                                        (format "Pub. ~a TRANSFERRED TO ~a"
                                                                                publisher-name (second (assoc new-owner-id owner-table)))))
                                                (query-exec db-conn
                                                            "UPDATE Publisher SET owner=? WHERE owner=?"
                                                            (if (= 0 new-owner-id)
                                                                sql-null
                                                                new-owner-id)
                                                            pub-id)
                                                (if (= 0 new-owner-id)
                                                    (query-exec db-conn "DELETE FROM Series WHERE publisher=?" pub-id)
                                                    (query-exec db-conn
                                                            "UPDATE Series SET publisher=? WHERE publisher=?"
                                                            new-owner-id
                                                            pub-id))
                                                (query-exec db-conn "DELETE FROM Publisher WHERE id=?" pub-id)
                                                (redirect-to "/admin-publishers")))))
           ))))
    (page
     (response/xexpr
      (if (not admin?)
          `(html (head (title "Employees Only!"))
                        (body (h1 "Employees Only!")
                              (a ((href "/")) "Return to main")))
                 (if (not publisher-data)
                     `(html (head (title "Invalid Publisher"))
                            (body (h1 "Invalid Publisher")
                                  (a ((href "/admin-publishers"))
                                     "Return to admin-publishers")))
                    `(html (head (title ,(format "Confirm ~a Deletion" publisher-name)))
                            (body (h1 ,publisher-name)
                                  (a ((href "/admin-publishers"))
                                     "Return to admin-publishers")
                                  (br)
                                  (a ((href "/admin-tools"))
                                     "Return to admin-tools")
                                  (h2 "4 possible deletion types")
                                  (ol
                                   (li (b "Delete All ") "Recursively delete all imprints and associated comics (parent goes out of business.)")
                                   (li (b "Transfer ") "First layer of imprints in hierarchy and all parent directly owned comics reparented to another company.")
                                   (li (b "Dissolve ") "First layer of imprints in hierarchy are orphaned and all parent directly owned comics deleted.")
                                   (li (b "Dissolve Recursive ") "Like above but all imprints made independent (can be done by user with repeated 3)
                                                                  and is not implemented here).")
                                   )
                                  (p "Before a Dissolve, make sure " (b "all surviving ") "comics owned by the dissolved company are
                                      reparented to their new owner " (b "or else they will be deleted"))
                                  (p (h2 "Warning: ") "Better to delete all associated subscriptions " (b "before a Delete All ")
                                                      "because MySQL triggers do not properly handle cascades and a manual fix of refunds must then be done")
                                  (form ([action ,(embed/url (lambda (inner-req)
                                                               (call-with-transaction db-conn
                                                                (lambda () ; cascade delete on the FK's of publisher and Series/Subscribable_Series should handle this
                                                                  (begin
                                                                    (query-exec db-conn "INSERT INTO Web_Transactions VALUES (?,NOW(),?,0)"
                                                                                user-id (format "Pub. ~a DELETE ALL" publisher-name))
                                                                    (query-exec db-conn "DELETE FROM Publisher WHERE id=?" pub-id)
                                                                    (redirect-to "/admin-publishers"))))))])
                                        (input ([type "submit"] [value "Delete All"])))
                                  (form ([action ,(embed/url (lambda (inner-req)
                                                               (formlet-process transfer-formlet inner-req)))])
                                        ,@(formlet-display transfer-formlet)
                                        (input ([type "submit"] [value "Transfer"])))
                                  (form ([action ,(embed/url (lambda (inner-req)
                                                               (redirect-to "/admin-publishers")))])
                                        (input ([type "submit"] [value "Cancel"])))
                                  ))
                     ))))))

(serve/servlet start
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:listen-ip "127.0.0.1"
               #:port 8080
               #:launch-browser? #f
               #:extra-files-paths (list (build-path "C:\\Users\\Thai Flowers\\Desktop\\ComicShop")))