(ns clj.core 
		(:require [clojure.data.priority-map :refer [priority-map]]
												[clojure.core.match :refer [match]]
												[clojure.math.numeric-tower :as math]
												[clojure.math.combinatorics :as combo]))


(defn multOf3Or5 [n]
		(apply + (filter #(or (= 0 (mod % 3)) (= 0 (mod % 5))) (range 0 n))))


;;(println (multOf3Or5 1000))


(defn lazyFib 
		([] (lazyFib 1 1))
		([a b] (lazy-seq (cons a (lazyFib b (+ a b))))))


(defn evenFib [n]
		(loop [s 0 f (lazyFib)]
				(let [v (first f)]
						(if (< v n)
								(if (even? v)
										(recur (+ s v) (rest f))
										(recur s (rest f)))
								s))))

;;(println (evenFib 4000000))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; MULTIMAP for SIEVE
;;;;;;;;;;;;;;;;;;;;;;;;;

(defn addSeq [m k v]
		(if-let [entries (get m k)]
				(assoc m k (conj entries v))
				(assoc m k [v])))

(defn stepSeqs [m k]
		(reduce (fn [mm iter]
													(addSeq mm (first iter) (rest iter)))
										(dissoc m k)
										(seq (get m k))))


;;;;;;;;;;;;;;;;;;;;;;;;
;; PRIME BULLSHIT
;;;;;;;;;;;;;;;;;;;;;;;;
(defn isMultiple [n sieve]
		(some #(= 0 (mod n %)) sieve))

(defn lazyTrialDivision
		([] (lazyTrialDivision #{} 2))
		([sieve n] (if (isMultiple n sieve)
																(lazy-seq (lazyTrialDivision sieve (inc n)))
																(lazy-seq (cons n (lazyTrialDivision (conj sieve n) (inc n)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;
;; SIEVE and the WHEEL
;;;;;;;;;;;;;;;;;;;;;;;;;


(defn spin [wheel start]
		(lazy-seq (cons start (spin (rest wheel) (+ start (first wheel))))))

(defn primeMultiples [p spinner]
		(lazy-seq (cons (* p (first spinner)) (primeMultiples p (rest spinner)))))

(defn sieve [guesses mm]
		(let [guess (first guesses)]
				(if-let [iter (first (get mm guess))]																	
						(sieve (rest guesses) (stepSeqs mm guess))
						(lazy-seq (cons guess 
																						(sieve (rest guesses)
																													(let [xs (primeMultiples guess guesses)]
																																(addSeq mm (first xs) (rest xs)))))))))

(defn lazyPrimes []
		(cons 2 (cons 3 (sieve (spin (cycle [2 4]) 5) {}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; P3
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn uniquePrimeFactors [n primes factors]
		(match [(> (first primes) (math/sqrt n)) (= 0 (mod n (first primes)))]
				[true _] (if (empty? factors) #{n} factors)
				[false true] (recur n (rest primes) (conj factors (first primes)))
				[false false] (recur n (rest primes) factors)))

(defn largestPF [n]
		(last (uniquePrimeFactors n (lazyPrimes) [])))

;;(println (largestPF 600851475143))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; P4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn getMost [elems isLegit compF]
		(reduce (fn [cur next]
												(if (and (isLegit next) (compF cur next))
														next
														cur))
										elems))

(defn isPalindrome [n]
		(= (seq (str n)) (reverse (str n))))

(defn largestPalindrome []
		(getMost (map #(* (first %) (second %)) (combo/cartesian-product (range 100 1000) (range 100 1000))) isPalindrome <))

;;(println (largestPalindrome))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; P5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn primeFactorization [n]
		(loop [cur n primes (lazyPrimes) factors []]
				(match [(= cur 1) (= 0 (mod cur (first primes)))]
						[true _] (if (empty? factors) [n] factors)
						[false true] (recur (/ cur (first primes)) primes (conj factors (first primes)))
						[false false] (recur cur (rest primes) factors))))

(defn pf->lcm [x y]
		(reduce #(assoc %1 %2 (max (x %2 0)(y %2 0))) 
										{} 
										(clojure.set/union (into #{} (keys x)) 
																													(into #{} (keys y)))))




(defn smallestMultiple [coll]
		(->> coll 
								(map (comp frequencies primeFactorization))
								(reduce pf->lcm)
								(reduce-kv #(* %1 (math/expt %2 %3)) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; P6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sumSqDiff [n]
		(- (let [x (apply + (range 1 (inc n)))] (* x x))
					(apply + (map #(* % %) (range 1 (inc n))))))
