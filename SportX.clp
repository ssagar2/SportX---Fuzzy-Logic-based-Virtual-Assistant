;;-------------------------------------------------------------
;; Templates
;;-------------------------------------------------------------

(deftemplate user
"User info"
  (slot name (default 0))
  (slot user-id (default 0)))
  
(deftemplate order
"Order info"
  (slot user-id)
  (slot ordernumber)
  (slot date)
  (slot item)
  (slot recom)
  (slot amount))

(deftemplate items
"Item details"
  (slot itemname)
  (slot quantity)
  (slot price)
  (slot rating)
  (slot durability))

(deftemplate question
"Questions that the system can ask"
  (slot text)
  (slot type)
  (slot id))

(deftemplate answer
  (slot id)
  (slot text))
  
(deftemplate cost-range
  (slot itemname)
  (slot range))
  
(deftemplate ratings
  (slot itemname)
  (slot rating))
  
(deftemplate item-durability
  (slot itemname)
  (slot durability))
  
;;-------------------------------------------------------------
;;Questions
;;-------------------------------------------------------------
  
(deffacts question-data
  "Questions the system can ask."
  (question (id existing-user) (type yes-no)
            (text "Are you a registered user?"))
  (question (id sign-up) (type yes-no)
            (text "Do you want to register for a new account?"))
  (question (id user-id) (type text)
            (text "Please provide your user-id/email address used to register."))
  (question (id user-details) (type text)
            (text "Please provide a user-id/email id you would like to register with."))
  (question (id order-status) (type yes-no)
            (text "Are you looking for details about your pending order status?"))
  (question (id order-no) (type yes-no)
            (text "Do you have the order number?"))
  (question (id order-number) (type string)
            (text "Can you please provide your order number so I can fetch the details from database?"))
  (question (id cancel-check) (type yes-no)
            (text "Do you wish to cancel your order?"))
  (question (id cancel-reason) (type string)
            (text "Sure. I can cancel your order. Before I proceed, may I know the reason for the order cancellation?"))
  (question (id modify-check) (type yes-no)
            (text "Okay! Thank you. You also have an option to modify the current order instead of cancellation. Do you wish to modify the order?"))
  (question (id cancel-confirm) (type yes-no)
            (text "Alright! Can I proceed with the cancellation?"))
  (question (id buy-item) (type text)
            (text "What would you like to buy?"))
  (question (id confirm-purchase) (type yes-no)
            (text "Do you confirm purchase?"))
  (question (id shop-more) (type yes-no)
            (text "Would you like to shop more?"))
  (question (id concerns) (type yes-no)
            (text "Do you have any other issues?"))
  (question (id queries) (type yes-no)
            (text "Do you have any other queries?"))
 )
            
  (defglobal ?*crlf* = "
")

;;-------------------------------------------------------------
;; Module ask
;;-------------------------------------------------------------


(defmodule ask)

(deffunction ask-user (?question ?type)
  "Ask a question and return the answer"
  (bind ?answer "")
  (while (not (is-of-type ?answer ?type)) do
         (printout t ?question " ")
         (if (eq ?type yes-no) then
           (printout t "(yes or no) "))
         (bind ?answer (read)))
  (return ?answer))

(deffunction is-of-type (?answer ?type)
  "Check if the answer is of the correct type"
  (if (eq ?type yes-no) then
    (return (or (eq ?answer yes) (eq ?answer no)))
    (elif (eq ?type number) then
           (return (numberp ?answer)))
    else (return (> (str-length ?answer) 0))))
   
(defrule ask::ask-question-by-id
  "Given the identifier of a question, ask the question and assert the answer"
  (declare (auto-focus TRUE))
  (MAIN::question (id ?id) (text ?text) (type ?type))
  (not (MAIN::answer (id ?id)))
  ?ask <- (MAIN::ask ?id)
  =>
  (bind ?answer (ask-user ?text ?type))
  (assert (answer (id ?id) (text ?answer)))
  (retract ?ask)
  (return))  
  

  
;;-------------------------------------------------------------
;; Module initialize
;;-------------------------------------------------------------

(defmodule initialize)
(import nrc.fuzzy.*)
(import nrc.fuzzy.jess.*)
(load-package nrc.fuzzy.jess.FuzzyFunctions)
(defglobal ?*costRange* = (new nrc.fuzzy.FuzzyVariable "cost" 0.0 100.0 "dollars"))
(defglobal ?*customerRating* = (new nrc.fuzzy.FuzzyVariable "rating" 0.0 10.0 "number"))
(defglobal ?*durability* = (new nrc.fuzzy.FuzzyVariable "durability" 1.0 100.0 "number"))
(defglobal ?*rlf* = (new nrc.fuzzy.RightLinearFunction))
(defglobal ?*llf* = (new nrc.fuzzy.LeftLinearFunction))

(defglobal ?*g-ordernum* = 0)
(defglobal ?*item* = 0)
(defglobal ?*uname* = 0)


(defrule init
   (declare (salience 100))
  =>
   (load-package nrc.fuzzy.jess.FuzzyFunctions)
   (?*costRange* addTerm "cheap" (new nrc.fuzzy.RFuzzySet 0.0 5.0 ?*rlf*))
   (?*costRange* addTerm "average" (new nrc.fuzzy.TrapezoidFuzzySet 4.0 4.8 5.5 6.0))
   (?*costRange* addTerm "expensive" (new nrc.fuzzy.LFuzzySet 5.5 6.0 ?*llf*))
   (?*customerRating* addTerm "low" (new nrc.fuzzy.RFuzzySet 0.0 2.0 ?*rlf*))
   (?*customerRating* addTerm "medium" (new nrc.fuzzy.TrapezoidFuzzySet 2.0 2.8 3.5 3.9))
   (?*customerRating* addTerm "high" (new nrc.fuzzy.LFuzzySet 3.5 5.0 ?*llf*))
   (?*durability* addTerm "6_months" (new nrc.fuzzy.RFuzzySet 1.0 8.0 ?*rlf*))
   (?*durability* addTerm "1_year" (new nrc.fuzzy.TrapezoidFuzzySet 7.0 10.8 15.5 22.0))
   (?*durability* addTerm "2_years" (new nrc.fuzzy.LFuzzySet 20.5 60.0 ?*llf*))

   (assert (items (itemname Cricet_bat) (quantity 1)
                   (price (new nrc.fuzzy.FuzzyValue ?*costRange*
                                (new nrc.fuzzy.PIFuzzySet 6.7 0.1)))
                   (rating (new nrc.fuzzy.FuzzyValue ?*customerRating*
                                (new nrc.fuzzy.PIFuzzySet 4.7 0.1)))
                   (durability (new nrc.fuzzy.FuzzyValue ?*durability*
                                (new nrc.fuzzy.PIFuzzySet 40.0 0.1)))
           )
           (items (itemname Baseball_bat) (quantity 1)
                   (price (new nrc.fuzzy.FuzzyValue ?*costRange*
                                (new nrc.fuzzy.PIFuzzySet 4.0 0.1)))
                   (rating (new nrc.fuzzy.FuzzyValue ?*customerRating*
                                (new nrc.fuzzy.PIFuzzySet 2.7 0.1)))
                   (durability (new nrc.fuzzy.FuzzyValue ?*durability*
                                (new nrc.fuzzy.PIFuzzySet 4.0 0.1)))
           )
           (items (itemname Tennis_racket) (quantity 1)
                   (price (new nrc.fuzzy.FuzzyValue ?*costRange*
                                (new nrc.fuzzy.PIFuzzySet 6.5 0.1)))
                   (rating (new nrc.fuzzy.FuzzyValue ?*customerRating*
                                (new nrc.fuzzy.PIFuzzySet 3.7 0.1)))
                   (durability (new nrc.fuzzy.FuzzyValue ?*durability*
                                (new nrc.fuzzy.PIFuzzySet 50.0 0.1)))
           )
           (items (itemname Badminton_racket) (quantity 1)
                   (price (new nrc.fuzzy.FuzzyValue ?*costRange*
                                (new nrc.fuzzy.PIFuzzySet 5.75 0.1)))
                   (rating (new nrc.fuzzy.FuzzyValue ?*customerRating*
                                (new nrc.fuzzy.PIFuzzySet 4.7 0.1)))
                   (durability (new nrc.fuzzy.FuzzyValue ?*durability*
                                (new nrc.fuzzy.PIFuzzySet 5.0 0.1)))
           )
           (items (itemname Cricket_ball) (quantity 1)
                   (price (new nrc.fuzzy.FuzzyValue ?*costRange*
                                (new nrc.fuzzy.PIFuzzySet 4.75 0.1)))
                   (rating (new nrc.fuzzy.FuzzyValue ?*customerRating*
                                (new nrc.fuzzy.PIFuzzySet 3.0 0.1)))
                   (durability (new nrc.fuzzy.FuzzyValue ?*durability*
                                (new nrc.fuzzy.PIFuzzySet 20.0 0.1)))
           )
           (items (itemname Baseball_ball) (quantity 1)
                   (price (new nrc.fuzzy.FuzzyValue ?*costRange*
                                (new nrc.fuzzy.PIFuzzySet 3.75 0.1)))
                   (rating (new nrc.fuzzy.FuzzyValue ?*customerRating*
                                (new nrc.fuzzy.PIFuzzySet 1.7 0.1)))
                   (durability (new nrc.fuzzy.FuzzyValue ?*durability*
                                (new nrc.fuzzy.PIFuzzySet 25.0 0.1)))
           )
           (items (itemname Tennis_ball) (quantity 1)
                   (price (new nrc.fuzzy.FuzzyValue ?*costRange*
                                (new nrc.fuzzy.PIFuzzySet 3.75 0.1)))
                   (rating (new nrc.fuzzy.FuzzyValue ?*customerRating*
                                (new nrc.fuzzy.PIFuzzySet 1.0 0.1)))
                   (durability (new nrc.fuzzy.FuzzyValue ?*durability*
                                (new nrc.fuzzy.PIFuzzySet 45.0 0.1)))
           )
		   (items (itemname Shuttle_cork) (quantity 1)
                   (price (new nrc.fuzzy.FuzzyValue ?*costRange*
                                (new nrc.fuzzy.PIFuzzySet 3.75 0.1)))
                   (rating (new nrc.fuzzy.FuzzyValue ?*customerRating*
                                (new nrc.fuzzy.PIFuzzySet 1.0 0.1)))
                   (durability (new nrc.fuzzy.FuzzyValue ?*durability*
                                (new nrc.fuzzy.PIFuzzySet 40.0 0.1)))
           )
   )
)
(defrule print-banner
  =>
  (printout t " " crlf)
  (printout t " " crlf)
  (printout t "WELCOME TO SoprtX the Online Sports equipment shop. I am your virtual assistant.!" crlf)
  (printout t "Please type your name and press Enter> ")
  (bind ?*uname* (read))
  (printout t crlf "***************************************************" crlf)
  (printout t " Hello, " ?*uname* "." crlf)  
  (printout t " Please answer a few questions and" crlf)
  (printout t " I can assist you with your issue today." crlf)
  (printout t "***************************************************" crlf crlf)
  (assert (user (name John) (user-id john)))
  (assert (user (name Sara) (user-id sara)))
  (assert (user (name Peter) (user-id pete)))
  (assert (user (name Kate) (user-id kate)))
  (assert (user (name Erik) (user-id erik)))
  (assert (order (user-id john) (ordernumber 123) (date 01/26/2017) (item Cricet_bat) (recom Cricket_ball)(amount 29.99)))
  (assert (order (user-id erik) (ordernumber 234) (date 01/27/2017) (item Baseball_bat) (recom Baseball_ball) (amount 39.99)))
  (assert (order (user-id pete) (ordernumber 345) (date 01/26/2017) (item Tennis_ball) (recom Tennis_ball) (amount 69.99)))
  (assert (order (user-id kate) (ordernumber 456) (date 01/27/2017) (item Baseball_bat) (recom Baseball_ball) (amount 39.99)))
  (assert (order (user-id sara) (ordernumber 567) (date 01/26/2017) (item Cricket_ball) (recom Cricet_bat) (amount 24.99)))
  (assert (order (user-id pete) (ordernumber 678) (date 01/27/2017) (item Cricet_bat) (recom Cricket_ball) (amount 29.99)))
  (assert (order (user-id john) (ordernumber 789) (date 01/26/2017) (item Cricet_bat) (recom Cricket_ball) (amount 29.99)))
  (assert (order (user-id sara) (ordernumber 890) (date 01/27/2017) (item Badminton_racket) (recom Shuttle_cork) (amount 9.99)))
  
)

  
;;-------------------------------------------------------------
;; Module rules
;;-------------------------------------------------------------


(defmodule rules)

(defrule identify-expensive-items
   (items (itemname ?n) (price ?ht&:(fuzzy-match ?ht "expensive")))
 =>
   (assert (cost-range (itemname ?n) (range EXPENSIVE)))
   ;;(printout t ?n " is expensive with degree (similarity) " (fuzzy-rule-similarity) crlf)
   ;;(printout t ?n " is expensive with degree (match) " (fuzzy-rule-match-score) crlf)
)

(defrule identify-cheap-items
   (items (itemname ?n) (price ?ht&:(fuzzy-match ?ht "cheap")))
 =>
   (assert (cost-range (itemname ?n) (range LESS-EXPENSIVE)))
)

(defrule identify-average-items
   (items (itemname ?n) (price ?ht&:(fuzzy-match ?ht "average")))
 =>
   (assert (cost-range (itemname ?n) (range AVERAGE)))
)

(defrule identify-low-rated-items
   (items (itemname ?n) (rating ?ht&:(fuzzy-match ?ht "low")))
 =>
   (assert (ratings (itemname ?n) (rating LOW)))
)

(defrule identify-medium-rated-items
   (items (itemname ?n) (rating ?ht&:(fuzzy-match ?ht "medium")))
 =>
   (assert (ratings (itemname ?n) (rating MEDIUM)))
)

(defrule identify-high-rated-items
   (items (itemname ?n) (rating ?ht&:(fuzzy-match ?ht "high")))
 =>
   (assert (ratings (itemname ?n) (rating HIGH)))
)

(defrule identify-small-sized-items
   (items (itemname ?n) (durability ?ht&:(fuzzy-match ?ht "6_months")))
 =>
   (assert (item-durability (itemname ?n) (durability 6_MONTHS)))
)

(defrule identify-medium-sized-items
   (items (itemname ?n) (durability ?ht&:(fuzzy-match ?ht "1_year")))
 =>
   (assert (item-durability (itemname ?n) (durability 1_YEAR)))
)

(defrule identify-large-sized-items
   (items (itemname ?n) (durability ?ht&:(fuzzy-match ?ht "2_years")))
 =>
   (assert (item-durability (itemname ?n) (durability 2_YEARS)))
)

(defrule inquire-user
  =>
  (assert (ask existing-user)))
  
(defrule not-an-existing-user
  (answer (id existing-user) (text ?st&:(eq ?st no)))
  =>
  (assert (MAIN::ask sign-up)))
  
(defrule existing-user
  (answer (id existing-user) (text ?st&:(eq ?st yes)))
  =>
  (assert (MAIN::ask user-id)))
  
(defrule user-id-check
  (answer (id user-id) (text ?user))
  ?u <- (user {user-id == ?user})
  =>
  (printout t "Details found! Username: " ?u.name "   User-id :" ?u.user-id crlf)
  (assert (MAIN::ask order-status)))
  
(defrule not-a-valid-userid
  (answer (id user-id) (text ?user))
  (not (user {user-id == ?user}))
  =>
  (printout t "Sorry. User-id not found!"  crlf)
  (assert (MAIN::ask existing-user))
  )
  
(defrule no-sign-up
  (answer (id sign-up) (text ?st&:(eq ?st no)))
  =>
  (assert (MAIN::ask shop-more)))
  
(defrule ask-sign-up
  (answer (id sign-up) (text ?st&:(eq ?st yes)))
  =>
  (assert (MAIN::ask user-details)))
  
(defrule new-user
  (answer (id user-details) (text ?user-id))
  =>
  (printout t "Username: " ?*uname* "   User-id :" ?user-id crlf)
  (assert (user (name ?*uname*) (user-id ?user-id)))
  (printout t "Account created successfully!" crlf)
  (assert (MAIN::ask order-status))
 )

(defrule request-order-status
  ;; check if user is requesting order status
  (answer (id order-status) (text ?st&:(eq ?st yes)))
  =>
  (assert (MAIN::ask order-no)))
  
(defrule not-requesting-order-status
  ;; check if user is requesting order status
  (answer (id order-status) (text ?st&:(eq ?st no)))
  =>
  (assert (MAIN::ask shop-more)))
  
(defrule request-order-number
  ;; Ask for the order number
  (answer (id order-no) (text ?st&:(eq ?st yes)))
  =>
  (assert (MAIN::ask order-number)))
  
(defrule not-requesting-order-number
  ;; Ask for the order number
  (answer (id order-no) (text ?st&:(eq ?st no)))
  =>
  (printout t "Sorry I can't help you without an order number.!" crlf)
  (printout t "Please call our customer service department at +1800-123-4567. Have a pleasant day!" crlf)
  (exit))

(defrule get-order-number
  (answer (id order-number) (text ?or_no))
  ?o <- (order {ordernumber == ?or_no})
  =>
  (bind ?*g-ordernum* ?o.ordernumber)
  (printout t "Your order: " ?o.ordernumber "   " ?o.date "   " ?o.item "   $" ?o.amount " " crlf)
  (printout t "Your order was received and will be dispatched in 1-2 business days.!" crlf)
  (printout t "I would recomend you to buy--.!" ?o.recom " " crlf)
  (assert (MAIN::ask cancel-check)))
  
(defrule invalid-order-no
  (and (answer (id order-number) (text ?or_no)) (not ?o <- (order {ordernumber == ?or_no})))
  =>
 (and (printout t "Order not found!!! " crlf) (assert (MAIN::ask order-number))))
  
(defrule ask-cancel-check
  (answer (id cancel-check) (text ?cc&:(eq ?cc yes)))
  =>
  (assert (MAIN::ask cancel-reason))
 )
 
(defrule no-cancel
  (answer (id cancel-check) (text ?cc&:(eq ?cc no)))
  =>
  (assert (MAIN::ask shop-more))
 )
 
(defrule purchase-item
  (answer (id buy-item) (text ?cc)) 
  ?s <- (item-durability {itemname == ?cc})
  ?n <- (cost-range {itemname == ?cc})
  ?r <- (ratings {itemname == ?cc})
  =>
  (printout t " " ?cc "  are priced " ?n.range " and are rated " ?r.rating crlf)
  (printout t " " ?cc "  comes with a " ?s.durability " warranty" crlf)
  (assert (MAIN::ask confirm-purchase))
)

(defrule invalid-item-name
  (answer (id buy-item) (text ?cc)) 
  (not (items {itemname == ?cc}))
  =>
  (printout t "Sorry! Item name incorrect!" crlf)
  (assert (MAIN::ask shop-more)))

(defrule confirm-order-purchase
  (answer (id confirm-purchase) (text ?cc&:(eq ?cc yes)))
  =>
  (printout t "Order placed! Your order will be dispatched in 2-3 days!" crlf)
  (assert (MAIN::ask concerns)))
  
(defrule decline-order-purchase
  (answer (id confirm-purchase) (text ?cc&:(eq ?cc no)))
  =>
  (assert (MAIN::ask concerns)))

(defrule call-phone-number
  (answer (id concerns) (text ?cc&:(eq ?cc yes)))
  =>
  (printout t "Please call our customer service department at 1800-123-4567. Have a pleasant day!" crlf)
  (exit))
  
(defrule no-concerns
  (answer (id concerns) (text ?cc&:(eq ?cc no)))
  =>
  (printout t "Thank you.! Have a pleasant day!" crlf)
  (exit))
 
(defrule ask-cancel-reason
  ;; Ask for the order number
  (answer (id cancel-reason) (text ?cr))
  =>
  (assert (MAIN::ask modify-check)))
  
(defrule ask-modify-check
  (answer (id modify-check) (text ?cc&:(eq ?cc no)))
  =>
  (assert (MAIN::ask cancel-confirm)))
  
(defrule cancel-confirmation
  (answer (id cancel-confirm) (text ?cc&:(eq ?cc no)))
  =>
  (assert (MAIN::ask shop-more)))
  
  
(defrule cancel-successful
  (answer (id cancel-confirm) (text ?cc&:(eq ?cc yes)))
  =>
   (printout t "Your order number " ?*g-ordernum* " has been cancelled.!" crlf)
   (printout t "You will receive the refund amount in 5-7 business days.!" crlf)
  (assert (MAIN::ask shop-more)))

(defrule modify-order
  (answer (id modify-check) (text ?cc&:(eq ?cc yes)))
  =>
  (assert (MAIN::ask shop-more)))
  
(defrule order-items
  (answer (id shop-more) (text ?cc&:(eq ?cc yes)))
  =>
  (printout t " " crlf)
  (printout t "Recommendations: "  crlf)
  (printout t " " crlf)
  (printout t "Cricet_bat"  crlf)
  (printout t "Baseball_bat"  crlf)
  (printout t "Tennis_racket"  crlf)
  (printout t "Badminton_racket"  crlf)
  (printout t "Cricket_ball"  crlf)
  (printout t "Baseball_ball"  crlf)
  (printout t "Tennis_ball"  crlf)
  (printout t "Shuttle_cork"  crlf)
  (printout t " " crlf)
  (assert (MAIN::ask buy-item))
)

(defrule close-session
  (answer (id shop-more) (text ?cc&:(eq ?cc no)))
  =>
   (assert (MAIN::ask concerns))
   )

;;-------------------------------------------------------------
;; Run
;;-------------------------------------------------------------

(deffunction start-assistant ()
  (reset)
  (focus initialize rules)
  (run))

(while TRUE
  (start-assistant))