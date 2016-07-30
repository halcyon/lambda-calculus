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
       (f (((lambda-num n) f) x))))))

;; (defn add [m n]
;;   (Lambda.
;;     (fn [f]
;;       (fn [x]
;;         (((lambda-num n) f) (((lambda-num m) f) x))))))

(defn add [m n]
  (Lambda.
    (fn [f]
      (fn [x]
        ((n f) ((m f) x))))))

;; (defn pow [m n]
;;   (Lambda.
;;    ((lambda-num n) (lambda-num m))))

(defn mult [m n]
  (Lambda.
   (fn [f]
     (fn [x]
       ((n (m f)) x)))))

(defn pow [m n]
  (Lambda.
   (fn [f]
     (fn [x]
       ((n (comp (m f) (m f))) x)))))

(defn lambda-num [n]
  (fn [f]
    (fn [x]
      ((apply comp (repeat n f)) x))))
