(ns lambda-calculus.core)

(defn zero [f]
  (fn [x] x))

(defn one [f]
  (fn [x]
    (f x)))

(defn two [f]
  (fn [x]
    (f (f x))))

(defn three [f]
  (fn [x]
    (f (f (f x)))))

(defn visual [x]
  (list 'f x))

(defn view [f]
  ((f visual) 'x))

(defn lambda-num [n]
  (fn [f]
    (fn [x]
      ((apply comp (repeat n f)) x))))

(defmacro def-lambda-num [n]
  `(fn [f#]
     (fn [x#]
       ((apply comp (repeat ~n f#)) x#))))

(defn def-lambda-num-num [n]
  `(~'fn [~'f]
     (~'fn [~'x]
      ((~'apply ~'comp ~(repeat n 'f)) ~'x))))

(defn eq? [m n]
  (= (view m) (view n)))

(deftype Lambda [f]
  Object
  (toString [_] (str (view f)))
  (equals [this other] (= (view this) (view other)))
  clojure.lang.IFn
  (invoke [this g]
    (f g)))

(defmethod print-method Lambda [v ^java.io.Writer w]
  (.write w (str v)))

(defn lambda-typed-num [n]
  (Lambda. (lambda-num n)))

(defn succ [n]
  (Lambda.
   (fn [f]
     (fn [x]
       (f ((n f) x))))))

(defn add [m n]
  (Lambda.
    (fn [f]
      (fn [x]
        ((m f) ((n f) x))))))

(defn mult [m n]
  (Lambda.
   (fn [f]
     (fn [x]
       ((m (n f)) x)))))

(defn mult2 [m n]
  (Lambda.
   (fn [f]
     (m (n f)))))

(defn pow [m n]
  (Lambda.
   (fn [f]
     ((n m) f))))

(defn T
  [x]
  (fn [y] x))

(defn F
  [x]
  (fn [y] y))

(defn view-bool [bool]
  ((bool "T") "F"))

(deftype LambdaBool [f]
  Object
  (toString [_] (str (view-bool f)))
  (equals [this other] (= (view-bool this) (view-bool other)))
  clojure.lang.IFn
  (invoke [this g]
    (f g)))

(defmethod print-method LambdaBool [v ^java.io.Writer w]
  (.write w (str v)))

(def T (->LambdaBool (fn [x]
                       (fn [y] x))))

(def F (->LambdaBool (fn [x]
                       (fn [y] y))))

(defn negation [x]
  (->LambdaBool
   ((x F) T)))

(defn negation-lambda [x]
  (->LambdaBool
   ((x (fn [x]
         (fn [y] y))) (fn [x]
                        (fn [y] x)))))

(defn conjunction [x]
  (->LambdaBool
   (fn [y]
     (->LambdaBool ((x y) F)))))

(defn disjunction [x]
  (->LambdaBool
   (fn [y]
     (->LambdaBool ((x T) y)))))

(defn factorial [n]
  (if (zero? n)
    1
    (* n (factorial (dec n)))))

(defn factorial-gen [f]
  (fn [n]
    (if (zero? n)
      1
      (* n (f (dec n))))))

(def factorial-gen
  (fn [f]
    (fn [n]
      (if (zero? n)
        1
        (* n (f (dec n)))))))

(defn factorial-weird [f]
  (fn [n]
    (if (zero? n)
      1
      (* n ((f f) (dec n))))))


(defn factorial-weird-expanded [f]
  (fn [n]
    (if (zero? n)
      1
      (* n ((fn [n]
              (if (zero? n)
                1
                (* n ((fn [n]
                        (if (zero? n)
                          1
                          (* n ((fn [n]
                                  (if (zero? n)
                                    1
                                    (* n ((fn [n]
                                            (if (zero? n)
                                              1
                                              (* n ((f f) (dec n)))))
                                          (dec n)))))
                                (dec n)))))
                      (dec n)))))
            (dec n))))))



(defn factorial-weird-expanded-one [f]
  (fn [n]
    (if (zero? n)
      1
      (* n ((fn [n]
              (if (zero? n)
                1
                (* n ((f f) (dec n))))) (dec n))))))

(def factorial-weird-weird
  ((fn [f]
     (fn [n]
       (if (zero? n)
         1
         (* n ((f f) (dec n))))))
   (fn [f]
     (fn [n]
       (if (zero? n)
         1
         (* n ((f f) (dec n))))))))

(((fn factorial [f]
    (f f))
  (fn [f]
    (fn [n]
      (if (zero? n)
        1
        (* n ((f f) (dec n))))))) 3)


(def Y (fn [f]
         ((fn [x]
            (x x))
          (fn [x]
            (f (fn [y]
                 ((x x) y)))))))

(def factorial-gen-for-y-combinator
  (fn [f]
    (fn [n]
      (if (zero? n)
        1
        (* n (f (dec n)))))))
