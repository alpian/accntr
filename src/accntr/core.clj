(ns accntr.core
  (:gen-class))

(require
  '[clojure.data.csv :as csv]
  '[clojure.java.io :as io])

(defn csv-data->maps [csv-data]
  (map zipmap
    (->> (first csv-data) ;; First row is the header
         (map keyword) ;; Drop if you want string keys instead
         repeat)
  	(rest csv-data)))

(defn -main
  "Read and categorize expenses"
  [& args]
  (with-open [reader (io/reader (first args))]
    (doall (->>
      (csv/read-csv reader)
      csv-data->maps
      (map (fn [csv-record]
             (println csv-record)))))))
