#lang racket

(define country-codes
  (make-hash
   '(("Albania" . "AL") ("Andorra" . "AD") ("Austria" . "AT")
     ("Belgium" . "BE") ("Bulgaria" . "BG") ("Croatia" . "HR")
     ("Cyprus" . "CY") ("Czech Republic" . "CZ") ("Denmark" . "DK")
     ("Estonia" . "EE") ("Finland" . "FI") ("France" . "FR")
     ("Germany" . "DE") ("Greece" . "GR") ("Hungary" . "HU")
     ("Iceland" . "IS") ("Ireland" . "IE") ("Italy" . "IT")
     ("Latvia" . "LV") ("Liechtenstein" . "LI") ("Lithuania" . "LT")
     ("Luxembourg" . "LU") ("Malta" . "MT") ("Monaco" . "MC")
     ("Netherlands" . "NL") ("North Macedonia" . "MK") ("Norway" . "NO")
     ("Poland" . "PL") ("Portugal" . "PT") ("Romania" . "RO")
     ("San Marino" . "SM") ("Slovakia" . "SK") ("Slovenia" . "SI")
     ("Spain" . "ES") ("Sweden" . "SE") ("Switzerland" . "CH")
     ("United Kingdom" . "GB") ("Vatican City" . "VA"))))

(define country-lengths
  (make-hash
   '(("AL" . 28) ("AD" . 24) ("AT" . 20) ("BE" . 16) ("BG" . 22)
     ("HR" . 21) ("CY" . 28) ("CZ" . 24) ("DK" . 18) ("EE" . 20)
     ("FI" . 18) ("FR" . 27) ("DE" . 22) ("GR" . 27) ("HU" . 28)
     ("IS" . 26) ("IE" . 22) ("IT" . 27) ("LV" . 21) ("LI" . 21)
     ("LT" . 20) ("LU" . 20) ("MT" . 31) ("MC" . 27) ("NL" . 18)
     ("MK" . 19) ("NO" . 15) ("PL" . 28) ("PT" . 25) ("RO" . 24)
     ("SM" . 27) ("SK" . 24) ("SI" . 19) ("ES" . 24) ("SE" . 24)
     ("CH" . 21) ("GB" . 22) ("VA" . 22))))

(define (get-country-code country)
  (if (= (string-length country) 2)
      country
      (hash-ref country-codes (string-titlecase country) #f)))

(define (get-iban-length country-code)
  (hash-ref country-lengths (string-upcase country-code) #f))

(define (random-4-digits)
  (+ 1000 (random 9000)))

(define (random-10-digits)
  (+ 1000000000
     (bitwise-ior
      (random 1073741824)  ; 2^30
      (arithmetic-shift (random 8) 30))))

(define (number-concat . args)
  (apply string-append (map number->string args)))

(define (make-bban bank-code account-number iban-length)
  (let* ((bban (number-concat bank-code account-number))
         (padding-length (- iban-length 4 (string-length bban))))
    (string-append (make-string padding-length #\0) bban)))

(define (swap-iban iban)
  (string-append
   (substring iban 4)
   (substring iban 0 4)))

(define (char->iban-num c)
  (if (char-alphabetic? c)
      (number->string (+ 10 (- (char->integer c) (char->integer #\A))))
      (string c)))

(define (make-iban-numeric iban)
  (apply string-append (map char->iban-num (string->list iban))))

(define (make-check-digits iban-numeric)
  (define (string-remainder str divisor)
    (define (char->digit c)
      (- (char->integer c) (char->integer #\0)))
    (define (process-digits rem index)
      (if (< index (string-length str))
          (let* ([c (string-ref str index)]
                 [digit (char->digit c)]
                 [new-rem (remainder (+ (* rem 10) digit) divisor)])
            (process-digits new-rem (add1 index)))
          rem))
    (process-digits 0 0))
  (let ((check (- 98 (string-remainder iban-numeric 97))))
    (if (< check 10)
        (string-append "0" (number->string check))
        (number->string check))))

(define (generate-iban country-code iban-length bank-code account-number)
  (let* ((bban (make-bban bank-code account-number iban-length))
         (iban (string-append country-code "00" bban))
         (iban-swapped (swap-iban iban))
         (iban-numeric (make-iban-numeric iban-swapped))
         (check-digits (make-check-digits iban-numeric)))
    (string-append
     country-code
     check-digits
     bban)))

(define (get-user-input)
  (displayln "Enter country name or country code: ")
  (read-line))

(define (validate-country-input input)
  (let ((country-code (get-country-code input)))
    (if country-code
        country-code
        (begin
          (displayln (format "Country not found: ~a" input))
          #f))))

(define (validate-iban-length country-code)
  (let ((iban-length (get-iban-length country-code)))
    (if iban-length
        iban-length
        (begin
          (displayln (format "IBAN length not found for country code: ~a" country-code))
          #f))))

(define (generate-and-display-iban country-code iban-length)
  (let* ((bank-code (random-4-digits))
         (account-number (random-10-digits))
         (iban (generate-iban
                (string-upcase country-code)
                iban-length
                bank-code
                account-number)))
    (displayln (format "Generated IBAN: ~a" iban))))

(define (main)
  (let* ((user-input (get-user-input))
         (country-code (validate-country-input user-input)))
    (when country-code
      (let ((iban-length (validate-iban-length country-code)))
        (when iban-length
          (generate-and-display-iban country-code iban-length))))))

(module+ main (main))
