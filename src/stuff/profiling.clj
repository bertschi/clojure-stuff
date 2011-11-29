;;; Simple profiling tool for use from the REPL
;;; Code follows Norvig's Common Lisp implementation

(ns stuff.profiling)

(defrecord ProfData [counts times call-stack])

(def ^:dynamic *profile-data*
     (atom (ProfData. {} {} (list))))

(defn- get-millisecond-time []
  (/ (double (. System nanoTime)) 1000000.0))

(defn- inc-profile-count [profile-data f-name]
  (assoc-in profile-data
	    [:counts f-name]
	    (inc (get-in profile-data [:counts f-name] 0))))

(defn- inc-profile-time [profile-data [_ t] f-name current-time]
  (assoc-in profile-data
	    [:times f-name]
	    (+ (get-in profile-data [:times f-name] 0)
	       (- current-time t))))

(defn- profile-enter [f-name current-time]
  (letfn [(update-time-of-caller [profile-data]
	    (if-let [entry (first (:call-stack profile-data))]
	      (inc-profile-time profile-data entry (first entry) current-time)
	      profile-data))]
    (swap! *profile-data*
	   (fn [profile-data]
	     (-> profile-data
		 (inc-profile-count f-name)
		 (update-time-of-caller)
		 (update-in [:call-stack] (fn [stack]
					    (cons [f-name current-time] stack))))))))

(defn- profile-exit [f-name current-time]
  (letfn [(update-time-of-caller [profile-data]
	    (if-let [[[f _] & stack] (seq (:call-stack profile-data))]
	      (assoc profile-data :call-stack
		     (cons [f current-time] stack))
	      profile-data))]
    (swap! *profile-data*
	   (fn [profile-data]
	     (-> profile-data
		 (inc-profile-time (first (:call-stack profile-data))
				   f-name current-time)
		 (update-in [:call-stack] rest)
		 (update-time-of-caller))))))

(defn profile-fn [f-name f]
  (fn [& args]
    (profile-enter f-name (get-millisecond-time))
    (let [result (apply f args)]
      (profile-exit f-name (get-millisecond-time))
      result)))

(defn- percent [x total]
  (/ (int (* (/ x total) (* 100 100))) 100))

;; ideally a pretty printer for tables should be used here ... but it works for now
(defn profile-report [profile-data]
  (let [profile-times (:times profile-data)
	ordered-f-names (map first (sort-by second > profile-times))
	total-time (reduce + (vals profile-times))]
    (println "Total elapsed time = " total-time "ms")
    (println " Count  Time [ms]  Time [%]  Name")
    (doseq [f-name ordered-f-names]
      (let [f-time (get profile-times f-name)]
	(println (get (:counts profile-data) f-name) " "
		 f-time " "
		 (percent f-time total-time) " "
		 f-name)))))

(defmacro with-profiling
  "Convenience macro for profiling. All functions named by fn-names are re-defined,
via with-redefs, to a profiled version and body is evaluated in this context."
  [fn-names & body]
  `(binding [*profile-data* (atom (ProfData. {} {} (list)))]
     (with-redefs [~@(mapcat (fn [f-name]
			       `(~f-name (profile-fn '~f-name ~f-name)))
			     fn-names)]
       (try 
	 ~@body
	 (finally (profile-report @*profile-data*))))))

(defn fns-in-ns
  "Finds all symbols the the given namespace which are bound to a function.
The second argument can be any of :all, :refers, :aliases, :imports, :interns or :publics.
It defaults to :publics."
  ([ns]
     (fns-in-ns ns :publics))
  ([ns which]
     (let [ns-mapping (cond (= which :all) (ns-map ns)
			    (= which :refers) (ns-refers ns)
			    (= which :aliases) (ns-aliases ns)
			    (= which :imports) (ns-imports ns)
			    (= which :interns) (ns-interns ns)
			    :else (ns-publics ns))]
       (for [[name f] ns-mapping
	     :when (and (var? f)
			(bound? f)
			(fn? (var-get f))
			(not (:macro (meta f))))]
	 name))))
