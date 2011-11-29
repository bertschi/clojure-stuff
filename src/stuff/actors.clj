;;; Erlang style actors implemented using clojure agents
;;; 
;;; Just a small test to explore the relationship between actors and agents
;;;

;;; The API consists of receive and send (!)

;;; first version: no mailbox ... non-matching messages are simply dropped

(ns stuff.actors)

;; receive blocks by returning a message handler for the agent
(defn receive-clauses [clauses body]
  (assert (even? (count clauses)) "receive takes an even number of clause exprs")
  (mapcat (fn [[e c]]
	    `[~e (do ~c ~@body)])
	  (partition 2 clauses)))

(defmacro receive [clauses & body]
  `(fn [old-handler# msg#]
     (case msg#
       ~@(receive-clauses clauses body)
       old-handler#))) ; by default the message is ignored and the old handler retained
       
(def ^:dynamic *actors* (atom {}))

(defn clear-actors []
  (reset! *actors* {}))

(defn ! [name msg]
  (assert (contains? @*actors* name))
  (send (get @*actors* name) (fn [code]
			       (when (fn? code)
				 (code code msg)))))

(defn spawn-named [name func & args]
  (let [actor (agent nil)]
    (swap! *actors* assoc name actor) ; make sure the actor exists before we run its code
    (send actor (fn [_] (apply func args)))
    (await actor) ; establish the initial handler synchronously
    name))

;;; Any function that calls receive can now be used to define an actor

;;; The famous ping-pong example to demonstrate what we have

(defn ping [n]
  (if (zero? n)
    (do (! :pong "finished")
	(println "Ping finished"))
    (do (! :pong "ping")
	(receive ["pong" (println "Ping " n " received pong")]
	  (ping (dec n))))))

(defn pong []
  (receive ["finished" (println "Pong is done")
	    "ping" (do (println "Pong received ping")
		       (! :ping "pong")
		       (pong))]))

(clear-actors)
(spawn-named :pong pong)
(spawn-named :ping ping 5)
