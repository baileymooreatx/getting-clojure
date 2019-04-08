;; Getting Clojure by Russ Olsen

; ; Chapter 1 Hello, Clojure
  ; Run lein REPL
; $ lein
  ; Type quit to exit

  ; Create and run new project
; $ ​​lein​​ ​​new​​ ​​app​​ ​​blottsbooks​
; $ cd blottsbooks
; $ ​​lein​​ ​​run​

  ; increment 
  (inc 1)  ; returns 2

  ; Symbols cannot contain ()[]{}@^ or start with a number
  (def​ first-name ​"Russ"​)
  ​(def​ the-average (/ (+ 20 40.0) 2.0))

  ; Functions
  ​(defn​ hello-world [] (println ​"Hello, world!"​))
  ​
  (defn​ say-welcome [what]
​ 	  (println ​"Welcome to"​ what))
  (say-welcome ​"Clojure"​) --> Welcome to Clojure


;; Chapter 2 Vectors and Lists

  ;Vectors
  [0 [1 [true 3 ​"four"​ 5] 6] 7]
​ 	(vector true 3 ​"four"​ 5) 

  (def​ novels [​"Emma"​ ​"Coma"​ ​"War and Peace"​])
​ 	(count novels)  ​; Returns 3.​
  (first novels)  ; Returns "Emma"
  (rest novels)   ; Returns ​(​"Coma"​ ​"War and Peace"​) NOT [​"Coma"​ ​"War and Peace"​]
  (conj novels ​"Carrie"​) ​; Adds "Carrie" to the end. [​"Emma"​ ​"Coma"​ ​"War and Peace"​ ​"Carrie"​].​
  (cons ​"Carrie"​ novels) ; Returns (​"Carrie"​ ​"Emma"​ ​"Coma"​ ​"War and Peace"​)

  ; Lists
  '(1 2 3)
  '(1 2 3 ​"four"​ 5 ​"six"​)
​ 	'(1 2.0 2.9999 ​"four"​ 5.001 ​"six"​)
​ 	'([1 2 (​"a"​ ​"list"​ ​"inside a"​ ​"vector"​)] ​"inside"​ ​"a"​ ​"list"​)

  (list 1 2 3 ​"four"​ 5 ​"six"​)

  ​(def​ poems '(​"Iliad"​ ​"Odyssey"​ ​"Now We Are Six"​))
​ 	
​ 	(count poems)   ​; Returns 3.​
​ 	(first poems)   ​; "Iliad".​
​ 	(rest poems)    ​; ("Odyssey" "Now We Are Six")​
​ 	(nth poems 2)   ​; "Now We Are Six".​


;; Chapter 3 Maps, Keywords, and Sets

  ; Maps
  ; There is a `map` function, which we’ll meet presently, but it does something different.
  ; Maps are immutable, like almost everything else. 
  (def​ book {​"title"​ ​"Oliver Twist"​
​ 	           ​"author"​ ​"Dickens"​
​ 	           ​"published"​ 1838})
  (hash-map ​"title"​ ​"Oliver Twist"​
​ 	          ​"author"​ ​"Dickens"​
​ 	          ​"published"​ 1838)
  (get book ​"published"​) ​; Returns 1838.​
  (book ​"published"​)     ; Returns 1838 also.
  
  ; Keywords
  ; Syntactically, a keyword literal starts with a colon and then follows the same rules as symbols.
  ; Keywords always stand for themselves—evaluate :title and you always get :title back
  :title
​ 	:author
​ 	:published
​ 	:word-count
​ 	:preface&introduction
​ 	:chapter-1-and-2

  (def​ book
​ 	  {:title ​"Oliver Twist"​ :author ​"Dickens"​ :published 1838})
​ 	
​ 	(println ​"Title:"​ (book :title))
​ 	(println ​"By:"​ (book :author))
​ 	(println ​"Published:"​ (book :published))

		; These are equivalent because a keyword is a function that returns itself.
  (book :title)
  ​(:title book)

  ; Add a key value pair. Returns a new map
  ​(assoc book :page-count 362) ; will return
  {:page-count 362
​ 	 :title ​"Oliver Twist"​
​ 	 :author ​"Dickens"​
​ 	 :published 1838}

		; Add and update a map. `assoc` performs both.
  (assoc book :page-count 362 :title ​"War & Peace"​)

  ; Remove key value pair(s)
  ; Quietly ignores any keys that aren’t actually in the map
  (dissoc book :published)
  (dissoc book :title :author :published)

  ; Retrieve keys in no particular order
  (keys book) ;returns
  ​(:title :author :published)

  ; Retrieve values in no particular order but will match `keys` order
  (vals book)
  (​"Oliver Twist" ​"Dickens" 1838)

  ​​; update - Start with 1,000 copies sold.​
​ 	(def​ book {:title ​"Emma"​ :copies 1000})
​ 	​; Now we have 1,001.​ update returns a new map
​ 	(def​ new-book (update book :copies inc))

  ; update-in - works like update but will also let you drill down 
  ; through several layers of maps using a pathlike vector of keys
  ​(def​ by-author
​ 	  {:name ​"Jane Austen"​
​ 	   :book {:title ​"Emma"​ :copies 1000}})
​ 	
​ 	(def​ new-by-author (update-in by-author [:book :copies] inc))

  ; Maps are the Swiss army knife of Clojure
  (require ​'clojure.java.jdbc​)
​ 	
​ 	(def​ db {:dbtype ​"derby"​ :dbname ​"books"​})
​ 	(clojure.java.jdbc/query db [​"select * from books"​]) ; would return
  ({:id 10, :title ​"Oliver Twist"​, :author ​"Dickens"​}
​ 	 {:id 20, :title ​"Emma"​, :author ​"Austen"​})

  (def​ db {:dbtype ​"MySQL"​
​ 	         :dbname ​"books"​
​ 	         :user ​"russ"​
​ 	         :password ​"noneofyourbeeswax"​})

  ; Sets
  (def​ genres #{:sci-fi :romance :mystery})
​ 	(def​ authors #{​"Dickens"​ ​"Austen"​ ​"King"​})

  (contains? authors ​"Austen"​)    ​; => true​
​ 	(contains? genres ​"Austen"​)     ​; => false​

  (authors ​"Austen"​)    ​; => "Austen"​
​ 	(genres :historical)  ​; => nil​

  ; if values are keywords, the keywords ARE functions
  (:sci-fi genres)      ​; => :sci-fi​	
​ 	(:historical genres)  ​; => nil​

		; add element
		(conj authors ​"Clarke"​)

		; remove element
		(disj more-authors ​"King"​)


;; Chapter 4 Logic

  ; The Fundamental If
  (defn​ print-greeting [preferred-customer]
​ 	  (​if​ preferred-customer
​ 	    (println ​"Welcome back to Blotts Books!"​))) ; then

  ; if else words like an inline ternary
	 (defn​ print-greeting [preferred-customer]
​ 	 (​if​ preferred-customer
​ 	   (println ​"Welcome back to Blotts Books!"​) ; then
​ 	   (println ​"Welcome to Blotts Books!"​)))    ; else does not require keyword

	 ; if returns the value of the last expression evaluated 
	 ; which is nill if the condition is false and no else is provided
 ​ (defn​ shipping-charge [preferred-customer order-amount]
​	 (​if​ preferred-customer
​ 	  0.00
​ 	  (* order-amount 0.10)))

  ; Asking Questions
  ; Boolean expressions can take any number of arguments
  (= 1 1)                           ​; True!​
​ 	(= 2 (+ 1 1))                     ​; True again!​
​​ 	(= ​"Anna Karenina"​ ​"Jane Eyre"​)   ​; Nope.​
​​ 	(= ​"Emma"​ ​"Emma"​)                 ​; Yes!​

  (= (+ 2 2) 4 (/ 40 10) (* 2 2) (- 5 1))  ​; True!​
​ 	(= 2 2 2 2 3 2 2 2 2 2) ​; False! There's a 3 in there.​

  (not= ​"Anna Karenina"​ ​"Jane Eyre"​)     ​; Yes!​
​ 	(not= ​"Anna Karenina"​ ​"Anna Karenina"​) ​; No!​

  (​if​ (> a b)
​ 	  (println ​"a is bigger than b"​))
​ 	
​ 	(​if​ (< b c)
​ 	  (println ​"b is smaller than c"​))

  ; Type testing
  (number? 1984)             ​; Yes!​
​ 	(number? ​"Anna Karenina"​)  ​; "Anna Karenina" isn't a number.​
​ 	(string? ​"Anna Karenina"​)  ​; Yes, it is a string.​
​ 	(keyword? ​"Anna Karenina"​) ​; Not a keyword.​
​ 	(keyword? :anna-karenina)  ​; Yes a keyword.​
​ 	(map? :anna-karenina)      ​; Not a map.​
​ 	(map? {:title 1984})       ​; Yes!​
​ 	(vector? 1984)             ​; Nope.​
​ 	(vector? [1984])           ​; Yes!​

  ; Charge extra if it's an express order or oversized​
​ 	​; and they are not a preferred customer.​
  ; NOTE: and and or short-circuit evaluation
​​ 	(defn​ shipping-surcharge? [preferred-customer express oversized]
​ 	  (and (not preferred-customer) (or express oversized)))


  ; Truthy and Falsy
  ; In an if statement and any other Boolean context, only false and nil 
  ; get treated as false. Everything else is treated as true. 
  (​if​ 0 ​"yes"​ ​"no"​) ​; Zero's not nil or false so "yes".​

  (​if​ [] (println ​"An empty vector is true!"​))
​ 	(​if​ [1 2 3] (println ​"So is a populated vector!"​))
​ 	
​ 	(​if​ {} (println ​"An empty map is true!"​))
​ 	(​if​ {:title ​"Make Room! Make Room!"​ }
​ 	  (println ​"So is a full map!"​))
​ 	
​ 	(​if​ () (println ​"An empty list is true!"​))
​ 	(​if​ '(:full :list) (println ​"So is a full list!"​))

  ; Do and When
  (do
​ 	  (println ​"This is four expressions."​)
​ 	  (println ​"All grouped together as one"​)
​ 	  (println ​"That prints some stuff and then evaluates to 44"​)
​ 	  44)

  (defn​ shipping-charge[preferred-customer order-amount]
​ 	  (​if​ preferred-customer
​ 	    (do
​ 	      (println ​"Preferred customer, free shipping!"​)
​ 	      0.0)
​ 	   (do ; else 
​ 	     (println ​"Regular customer, charge them for shipping."​)
​ 	     (* order-amount 0.10))))

  ; Clojure also sports a variant of if called when, 
  ; which doesn’t have an else (or falsy) leg but which supports 
  ; multiple statements without needing the do.
  (when preferred-customer
​ 	  (println ​"Hello returning customer!"​)
​ 	  (println ​"Welcome back to Blotts Books!"​))

  ; Dealing with Multiple Conditions
  ; nested ifs
  (defn​ shipping-charge [preferred-customer order-amount]
​ 	  (​if​ preferred-customer
​ 	    0.0
​ 	    (​if​ (< order-amount 50.0)
​ 	      5.0
​ 	      (​if​ (< order-amount 100.0)
​ 	        10.0
​ 	        (* 0.1 order-amount)))))

  ; cond is easier to read
  (defn​ shipping-charge [preferred-customer order-amount]
​ 	  (​cond​
​ 	    preferred-customer 0.0      ; if
​ 	    (< order-amount 50.0) 5.0   ; else if
​ 	    (< order-amount 100.0) 10.0 ; else if
​ 	    (>= order-amount 100.0) (* 0.1 order-amount))) ;else

  ; This is equivalent because the keyword is always true
  (defn​ shipping-charge [preferred-customer order-amount]
​ 	  (​cond​
​ 	    preferred-customer 0.0
​ 	    (< order-amount 50.0) 5.0
​ 	    (< order-amount 100.0) 10.0
​ 	    :else (* 0.1 order-amount)))

  ; case statement
  (defn​ customer-greeting [status]
​ 	  (case status
​ 	    :gold       ​"Welcome, welcome, welcome back!!!"​
​ 	    :preferred  ​"Welcome back!"​
​ 	                ​"Welcome to Blotts Books"​)) ; default case is last and unlabeled

  ; Throwing and Catching
  (try
​ 	  (publish-book book)
​ 	  (catch ArithmeticException e (println ​"Math problem."​))
​ 	  (catch StackOverflowError e (println ​"Unable to publish.."​)))

  ; Built-in  ex-info function takes a string describing 
  ; the problem and a (possibly empty) map containing any other pertinent information
  ; Throws clojure.lang.ExceptionInfo
  (defn​ publish-book [book]
​ 	  (when (not (:title book))
​ 	    (throw
​ 	      (ex-info ​"A book needs a title!"​ {:book book})))
​ 	
​ 	  ​  ;; Lots of publishing stuff...​
​ 	  )


;; 5 More Capable Functions

  ; One Function, Different Parameters
  ; Multi-arity functions - silimar to polymorphism except more versatile
  ; overloads are defined in a single function
  ; call one arity from the other to avoid duplicaton
  (defn​ greet
​ 	  ([to-whom] (greet ​"Welcome to Blotts Books"​ to-whom)) ; note recursion
​ 	  ([message to-whom] (println message to-whom)))

  ; Arguments with Wild Abandon
  ; variadic functions allow any number of arguments
  (defn​ print-any-args [& args]
​ 	  (println ​"My arguments are:"​ args))

 	; We can have ordinary arguments before the &, 
 	; so that we can rewrite first-argument like this:
 	​(defn​ new-first-argument [x & args] x)

  ; both multi-arity and variadic - signatures must be distinct
	 (defn​ one-two-or-more
	​ 	 ([a] (println ​"One arg:"​ a))
	​ 	 ([a b] (println ​"Two args:"​ a b))
	​ 	 ([a b & more] (println ​"More than two:"​ a b more)))

  ; Multimethods
  ; Change normalization functions into multimethods
 	(defn​ dispatch-published [book]
​ 	  (​cond​
​ 	    (< (:published book) 1928) :public-domain
​ 	    (< (:published book) 1978) :old-copyright
​ 	    :else :new-copyright))

 	; can be rewritten as a Multimethod
​ 	(defmulti​ compute-royalties dispatch-published)
​ 	
​ 	(defmethod​ compute-royalties :public-domain [book] 0)
​ 	
​ 	(defmethod​ compute-royalties :old-copyright [book]
​ 	  ​;; Compute royalties based on old copyright law.​
​ 	  )
​ 	
​ 	(defmethod​ compute-royalties :new-copyright [book]
​ 	  ​;; Compute royalties based on new copyright law.​
​ 	  )
 
 ; Deeply Recursive
 ; vector recursion
	(def​ books
	​ 	  [{:title ​"Jaws"​  :copies-sold 2000000}
	​ 	   {:title ​"Emma"​  :copies-sold 3000000}
	​ 	   {:title ​"2001"​  :copies-sold 4000000}])

	(defn​ sum-copies
	​ 	  ([books] (sum-copies books 0))
	​ 	  ([books total]
	​ 	    (​if​ (empty? books)
	​ 	      total
	​ 	      (sum-copies
	​ 	        (rest books)
	​ 	        (+ total (:copies-sold (first books)))))))

	(sum-copies books)
	  --> (sum-copies [{:title ​"Jaws"​  :copies-sold 2000000} {:title ​"Emma" :copies-sold 3000000} {:title ​"2001"​  :copies-sold 4000000}] 0)
	  --> (sum-copies [({:title ​"Emma" :copies-sold 3000000} {:title ​"2001"​  :copies-sold 4000000}) 2000000])
	  --> (sum-copies [({:title ​"2001"​  :copies-sold 4000000}) 5000000])
	  --> (sum-copies [() 9000000])
	  --> 9000000

 ; `recur` is less memory intensive
	(defn​ sum-copies [books]
	​ 	  (loop [books books total 0]
	​ 	    (​if​ (empty? books)
	​ 	      total
	​ 	      (recur
	​ 	        (rest books)
	​ 	        (+ total (:copies-sold (first books)))))))

  ; elegant solution using apply where	(apply [function arg] (function arg))
 	(defn​ sum-copies [books] (apply + (map :copies-sold books)))


 	; Docstrings
 	; A documentation string—or docstring for short—is a regular string that you can 
 	; insert just after the function name in your defn:
​ 	(defn​ average
​ 	  ​"Return the average of a and b."​
​ 	  [a b]
​ 	  (/ (+ a b) 2.0))

  ; Pre and Post Conditions 
	 (defn​ publish-book [book]
	​ 	 {:pre  [(:title book) (:author book)]
	​ 	  :post [(boolean? %)]}
	​ 	 (print-book book)
	​ 	 (ship-book book))

  (defn​ =
​ 	  ​"Equality. Returns true if x equals y, false if not. Same as​
​ 	​  Java x.equals(y) except it also works for nil, and compares​
​ 	​  numbers and collections in a type-independent manner.​
​ 	​  Clojure's immutable data structures define equals()​
​ 	​  (and thus =) as a value, not an identity, comparison."​
​ 	  ([x] true)
​ 	  ([x y] (clojure.lang.Util/equiv x y))
​ 	  ([x y & more]
​ 	   (​if​ (clojure.lang.Util/equiv x y)
​ 	     (​if​ (next more)
​ 	       (recur y (first more) (next more))
​ 	       (clojure.lang.Util/equiv y (first more)))
​ 	     false)))

;; Chapter 6 Functional Things

  ; Functions are values 
  ​(def​ dracula {:title ​"Dracula"​
​ 	              :author ​"Stoker"​
​ 	              :price 1.99
​ 	              :genre :horror})

  (defn​ cheap? [book]
​ 	  (when (<= (:price book) 9.99)
​ 	    book))
​ 	(defn​ pricey? [book]
​ 	  (when (> (:price book) 9.99)
​ 	    book))
​ 	
​ 	(cheap? dracula)        ​; Yes!​
​ 	(pricey? dracula)       ​; No!​

  (defn​ horror? [book]
​ 	  (when (= (:genre book) :horror)
​ 	    book))
​ 	
​ 	(defn​ adventure? [book]
​ 	  (when (= (:genre book) :adventure)
​ 	    book))
​ 	
​ 	(horror? dracula)       ​; Yes!​
​ 	(adventure? dracula)    ​; Nope!​

  (defn​ cheap-horror? [book]
​ 	  (when (and (cheap? book)
​ 	             (horror? book))
​ 	    book))
​ 	(defn​ pricy-adventure? [book]
​ 	  (when (and (pricey? book)
​ 	             (adventure? book))
​ 	    book))

  ; assign a function an alias
  (def​ reasonably-priced? cheap?)

  ​(defn​ both? [first-predicate-f second-predicate-f book]
​ 	  (when (and (first-predicate-f book)
​ 	             (second-predicate-f book))
​ 	    book))
​ 	
​ 	(both? cheap? horror? dracula)     ​; Yup!​	
​ 	(both? pricey? adventure? dracula) ​; Nope!​

  ; Functions on the Fly
  ; dynamically created functions and anonymous functions
	 (def​ double-it (​fn​ [n] (* 2 n)))

	 (defn​ cheaper-f [max-price]
​ 	  (​fn​ [book]
​ 	    (when (<= (:price book) max-price)
​ 	      book)))

	 (def​ real-cheap? (cheaper-f 1.00))
​  (def​ kind-of-cheap? (cheaper-f 1.99))
​  (def​ marginally-cheap? (cheaper-f 5.99))


  ; Functions that return functions
  ​(defn​ both-f [predicate-f-1 predicate-f-2]
​ 	  (​fn​ [book]
​ 	    (when (and (predicate-f-1 book) (predicate-f-2 book))
​ 	      book)))
  
  (def​ cheap-horror? (both-f cheap? horror?)) 	
​ 	(def​ real-cheap-adventure? (both-f real-cheap? adventure?))
​ 	(def​ real-cheap-horror? (both-f real-cheap? horror?))

  ; A Functional Toolkit
  ; apply -  You supply a function and a collection of arguments, 
  ; and apply will call that function with the arguments, returning the result
  (def​ the-function +)
​ 	(def​ args [1 2 3 4])

  (apply the-function args) ​; (the-function args0 args1 args2 ...)​

  ; partial - Each call to partial there is giving us back a new function 
  ; that—when called—calls cheaper-than with one of the prices as the first argument.
	 (defn​ cheaper-than [max-price book]
	​ 	  (when (<= (:price book) max-price)
	​ 	    book))
	​ 	
	​ (def​ real-cheap? (partial cheaper-than 1.00))
	​ (def​ kind-of-cheap? (partial cheaper-than 1.99))
	​ (def​ marginally-cheap? (partial cheaper-than 5.99))

  ; complement wraps the function that you supply with a call to not
  ; (complement [function] (not function))
  (def​ not-adventure? (complement adventure?))

  ; every-pred combines predicate functions into a single function that ands them all together
  (def​ cheap-horror-possession?
​ 	  (every-pred
​ 	    cheap?
​ 	    horror?
​ 	    (​fn​ [book] (= (:title book) ​"Possession"​))))

   ; Function Literals (aka Lambdas)
   ; Note there are no named arguments in function literals; 
   ; instead they use the very shell script-ish notation of
   ; %1 to stand for the first argument, %2 for the second argument, and so on.
   #(when (= (:genre %1) :adventure) %1)
   #(* 2 %1)
   #(+ %1 %2 %3)

   ; Number can be ommitted for one argument functions
   #(* % 2)

   ; pure function: a function that neither relies on nor generates side effects.

;; Chapter 7 Let

  ; A Loal, Temporary Place for Your Stuff
  ; let - returns the value of the last statement evaluated
  (defn​ compute-discount-amount [amount discount-percent min-charge]
​ 	  (let​ [discounted-amount (* amount (- 1.0 discount-percent))]
​ 	    (​if​ (> discounted-amount min-charge)
​ 	      discounted-amount
​ 	      min-charge)))

​  (defn​ compute-discount-amount [amount discount-percent min-charge]
​ 	  (​let​ [discount (* amount discount-percent)
​ 	        discounted-amount (- amount discount)]
​ 	    (println ​"Discount:"​ discount)
​ 	    (println ​"Discounted amount"​ discounted-amount)
​ 	    (​if​ (> discounted-amount min-charge)
​ 	      discounted-amount
​ 	      min-charge)))

  ; Let Over Fn
  (def​ user-discounts
​ 	  {​"Nicholas"​ 0.10 ​"Jonathan"​ 0.07 ​"Felicia"​ 0.05})

  (defn​ mk-discount-price-f [user-name user-discounts min-charge]
​ 	  (​let​ [discount-percent (user-discounts user-name)]
​ 	    (​fn​ [amount]
​ 	      (​let​ [discount (* amount discount-percent)
​ 	            discounted-amount (- amount discount)]
​ 	        (​if​ (> discounted-amount min-charge)
​ 	          discounted-amount
​ 	          min-charge)))))

  ;; Get a price function for Felicia.​
​ 	(def​ compute-felicia-price (mk-discount-price-f ​"Felicia"​ user-discounts 10.0))
​ 	
​ 	​;; ...and sometime later compute a price​
​ 	(compute-felicia-price 20.0)

  ;Variations on the Theme
  ; if-let
  ; if the book has an author return the author's name in uppercase
  ; else return "ANONYMOUS"
  (defn​ uppercase-author [book]
​ 	  (if-let [author (:author book)]
​ 	    (.toUpperCase author)
​ 	    ​"ANONYMOUS"​))

  ; when-let
  ; execute any number of statements
  ; does not support else
  (defn​ uppercase-author [book]
​ 	  (when-let [author (:author book)]
​ 	    (.toUpperCase author)))


;;  Chapter 8 Def, Symbols, and Vars

  ; A Global, Stable Place for Your Stuff
  ; We say that def binds a symbol to a value.
  ; Define global constants
  (def​ PI 3.14)
  (def​ ISBN-LENGTH 13)
  (def​ OLD-ISBN-LENGTH 10)

  (def​ isbn-lengths [OLD-ISBN-LENGTH ISBN-LENGTH])
  (defn​ valid-isbn [isbn]
​ 	  (or (= (count isbn) OLD-ISBN-LENGTH)
​ 	      (= (count isbn) ISBN-LENGTH)))

  ; Symbols Are Things
  (def​ author ​"Austen"​) ; binds the value "Austin" to the symbol author
  'author​ ​; The symbol author, not the string "Austen"​
  (str ​'author​) ​; The string "author".​
  ; compare symbols
  (= ​'author​ ​'some-other-symbol​)  ​; Nope.​
​ 	(= ​'title​ ​'title​)               ​; Yup.​

  ; Bindings Are Things Too
  ; bindings are called vars. vars contain a reference to the symbol and its value.
  (def​ author ​"Austen"​)  ​; Make a var.​
​ 	#​'author​               ​; Get at the var for author -> "Austen".​
  (def​ the-var #​'author​) ​; Grab the var.​
  ; use the API to get the symbol and its value
  (.get the-var)         ​; Get the value of the var: "Austen"​
​ 	(.-sym the-var)        ​; Get the symbol of the var: author​

  ; This will fail because let does not allocate vars
​ 	(​let​ [let-bound 42] #​'let-bound​)
  
  ; Varying Your Vars
  ; binding allows one to change a dynamic symbol's value temporarily
  ;; Make debug-enabled a dynamic var.​
​ 	(def​ ^:dynamic *debug-enabled* false)
​ 	
​ 	(defn​ debug [msg]
​ 	  (​if​ *debug-enabled*
​ 	    (println msg)))
​ 	
​ 	(binding [*debug-enabled* true]
​ 	  (debug ​"Calling that darned function"​)
​ 	  (some-troublesome-function-that-needs-logging)
​ 	  (debug ​"Back from that darned function"​))

  ; You can changed the valu of a dynamic symbol instide a binding
  (set! *print-length* 2)

  ; *1 is like bash ? - it returns the last result evaluated
  ; Similarly, *2 is the second to last result
  ; *3 is the one before that.
  (+ 2 2) ; returns 4
  *1      ; returns 4

  ; *e binds to the last exception
  *e
​ 	#error {
​ 	 :cause ​"Divide by zero"​
​ 	 :via
​ 	 [{:type java.lang.ArithmeticException
​ 	   :message ​"Divide by zero"​
​ 	   :at [clojure.lang.Numbers divide ​"Numbers.java"​ 158]}]
​ 	 :trace
​ 	 [[clojure.lang.Numbers divide ​"Numbers.java"​ 158]
​ 	 << And so on ​for​ quite some time... >>


;;  Chapter 9 Namespaces

  ; A Place for Your Vars
  ; A Clojure namespace is just a big lookup table of vars, indexed by their symbols.
  ; Clojure's default namespace is `user`

  ; Create a namespace if it does not already exist
  (ns pricing)
​ 	
​ 	(def​ discount-rate 0.15)
​ 	(defn​ discount-price [book]
​ 	  (* (- 1.0 discount-rate) (:price book)))

  ; If the namespace already exists, we switch to it
  (ns user)

​ 	​; Back to the pricing namespace.​
​ 	(ns pricing)

  (println (discount-price {:title ​"Emma"​ :price 9.99}))

  ; access elements from a different namespace
  (ns user)

  (println (pricing/discount-price {:title ​"Emma"​ :price 9.99}))

  ; Loading Namespaces
  ​(require ​'clojure.data​)  ; NOTE symbol with tick for REPL

   (​def​ literature [​"Emma"​ ​"Oliver Twist"​ ​"Possession"​])
   (​def​ horror [​"It"​ ​"Carry"​ ​"Possession"​])

   ​(clojure.data/diff literature horror)
   ; returns ​ 	[[​"Emma"​ ​"Oliver Twist"​] [​"It"​ ​"Carrie"​] [nil nil ​"Possession"​]]

   ; A Namespace of Your Own
   ; /src/blottsbooks/pricing.clj
   (ns blottsbooks.pricing)
​ 	
   (def​ discount-rate 0.15)
​ 	
​ 	 (defn​  discount-price [book]
​ 	   (- (:price book)
​ 	     (* discount-rate (:price book))))

  ; /src/blottsbooks/core.clj
  ​(ns blottsbooks.core
​ 	(:require blottsbooks.pricing) ; NOTE keyword with no tick in project
​ 	(:gen-class))
​ 	
​ 	(defn​ -main []
​ 	  (println
​ 	    (blottsbooks.pricing/discount-price {:title ​"Emma"​ :price 9.99})))

  ; As and Refer
  ; alias namespace with :as
  ​(require '[blottsbooks.pricing :as pricing])   ; REPL

  (ns blottsbooks.core                           ; Project
​ 	  (:require [blottsbooks.pricing :as pricing])
​ 	  (:gen-class))

  ; alias function with :refer
  ; Use :refer very sparsely because you can also accidentally
  ; overwrite standard, Clojure-supplied functions
  (require '[blottsbooks.pricing :refer [discount-price]])

  ; instead of (pricing/discount-price {:title ​"Emma"​ :price 9.99})
  (discount-price {:title ​"Emma"​ :price 9.99})

  ;; REPLs generally include the name of the current namespace 
  ;; in their prompts. If you start a REPL with Leiningen outside of
  ;; a project directory, your initial namespace will be user, 
  ;; and that’s what you will see in your prompt. 
  ;; On the other hand, if you start a REPL from inside of a Clojure 
  ;; project directory, Leiningen will default to the core namespace of that project. 

  ; Namespaces, Symbols, and Keywords
  ; show current namespace
  (println ​"Current ns:"​ *ns*)
  ; Returns: Current ns​:​ #object[clojure.lang.Namespace 0x76c706bf user]

  ; look up existing namespace
  (find-ns ​'user​) ​; Get the namespace called 'user.​

  ; show the content of a namespace
  (ns-map (find-ns ​'user​)) ​; Includes all the predefined vars.​
  (ns-map ​'user​)           ; shorter form of previous

  ; This will return "pricing"
  ​(namespace ​'pricing/discount-print​)

  ; Keywords also have room for a namespace, which you can add by either 
  ; explicitly calling out the namespace:
​ 	:blottsbooks.pricing/author

  ; or by doubling up the colon in the front of the keywork
  ; which will pick up the current namespace. Adding a namespace to a keyword 
  ; is mainly about preventing keyword collisions. Thus if you are worried that your
  ; :book may be confused with someone else’s :book, you can always slap an extra colon on it
​ 	::author

  ; After Clojure creates a new namespace, it does the equivalent of this:
​ 	(require '[clojure.core :refer :all])

  ; To see all the contents of clojure.core run this
  (ns-map ​'clojure.core​)

  ; Namespaces have no heirarchy. Clojure sees clojure.core and clojure.core.data
  ; as independent, unrelated entities.

  ; After making changes to a required namespace, the namespace must be reloaded
  ; for the changes to be in scope.
  (require :reload '[blottsbooks.pricing :as pricing])

  ; The previous contents of the namespace are still in scope unless you force
  ; namespace symbols to be unloaded from memory by unmapping it.
  (ns-unmap ​'blottsbooks.pricing​ ​'discount-price​)

  ; To prevent symbols from being re-evaluated when a namespace is reloaded use
  ;; Just set some-value the first time.​
​ 	(defonce some-value (function-with-side-effects))

  ; To force a defonce symbol to be reloaded use
  (ns-unmap *ns* ​'some-value​)


;;  Chapter 10 Sequences

  ; One Thing After Another - sequence adaptor
  ​(def​ title-seq (seq [​"Emma"​ ​"Oliver Twist"​ ​"Robinson Crusoe"​]))
  ; returns sequence (​"Emma"​ ​"Oliver Twist"​ ​"Robinson Crusoe"​) this NOT a list

  (seq {:title ​"Emma"​, :author ​"Austen"​, :published 1815})
  ; returns ([:title ​"Emma"​] [:author ​"Austen"​] [:published 1815])

  (seq (seq [​"Red Queen"​ ​"The Nightingale"​ ​"Uprooted"​]))
  ; this is a no-op that returns the same sequence

  ; Applying seq to an empty collection returns nil
  ; We can use (seq collection) as a truthy value to detect empty collections
  (seq [])   ​; Gives you nil.​​
  (seq '())  ​; Also nil.​​
  (seq {})   ​; Nil again.

  ​; A Universal Interface
  (first (seq '(​"Emma"​ ​"Oliver Twist"​ ​"Robinson Crusoe"​))) ; "Emma"
  (rest (seq '(​"Emma"​ ​"Oliver Twist"​ ​"Robinson Crusoe"​)))  ; (​"Oliver Twist"​ ​"Robinson Crusoe"​)
  (next (seq '(​"Emma"​ ​"Oliver Twist"​ ​"Robinson Crusoe"​)))  ; (​"Oliver Twist"​ ​"Robinson Crusoe"​)

  (rest '())  ; ()
  (next '())  ; nil

  (cons ​"Emma"​ (seq '(​"Oliver Twist"​ ​"Robinson Crusoe"​)))  ; (​"Emma"​ ​"Oliver Twist"​ ​"Robinson Crusoe"​)

  (defn​ my-count [col]
​ 	  (​let​ [the-seq (seq col)]
​ 	    (loop [n 0 s the-seq]
​ 	      (​if​ (seq s)
​ 	        (recur (inc n) (rest s))
​ 	        n))))

  ; functions like rest, next, and cons always (aside from the occasional nil) return sequences
​ 	(rest [1 2 3])                          ​; A sequence!​
​ 	(rest {:fname ​"Jane"​ :lname ​"Austen"​})  ​; Another sequence.​
​ 	(next {:fname ​"Jane"​ :lname ​"Austen"​})  ​; Yet another sequence.​
​ 	(cons 0 [1 2 3])                        ​; Still another.​
​ 	(cons 0 #{1 2 3})                       ​; And another.​

  ; A Rich Toolkit
  