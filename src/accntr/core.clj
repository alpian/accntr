(ns accntr.core
  (:gen-class))

(require
  '[clojure.data.csv :as csv]
  '[clojure.java.io :as io])

(def categorization [
  [#"^WOOLWORTHS.*$" :woolworths [:food]]
  [#"^PnP Fam Welge.*$" :picknpay [:food]]
  [#"^PANORAMA VETE.*$" :panorama-vet [:dogs]]
  [#"^ITUNES\.COM.*$" :itunes [:entertainment]]
  [#"^UBER SA [help|HELP].*$" :uber [:taxi]]
  [#"^MWEB IN   INTERNET.*$" :mweb [:internet]]
  [#"^272713422 VOD PREPA.*$" :vodacom [:phone]]
  [#"^VOD PREPAID    0791716430.*$" :vodacom [:phone]]
  [#"^PAM GOLDING.*$" :pam-golding [:rent]]
  [#"^I SnapScan Ecc.*$" :ecco [:food]]
  [#"^SUPERSPAR WELG.*$" :spar [:food]]
  [#"^I SnapScan Del.*$" :deluxe [:food]]
  [#"^ABSOLUTE PETS.*$" :absolute-pets [:dog-food]]
  [#"^VOD PREPAID    0713369497.*$" :vodacom [:phone]]
  [#"^Clicks Gardens.*$" :clicks [:medication]]
  [#"^FIXED MONTHLY FEE 272713422.*$" :standard-bank [:bank-fees]]
  [#"^ELEC           01023614165.*$" :eskom [:electricity]]
  [#"^DIS-CHEM [TYGER|TABS].*$" :dis-chem [:toiletries]]
  [#"^MONIQUE.*$" :monique [:baby-sitting]]
  [#"^LEANDRI.*$" :leandri [:baby-sitting]]
  [#"^WELGEMOED ANIM.*$" :welgemoed-anim [:dogs]]
  [#"^272713422 ELEC      01023614165.*$" :dis-chem [:toiletries]]
  [#"^OUMEUL WILABS.*$" :ou-meul [:food]]
  [#"^PNP KENRIDGE.*$" :picknpay [:food]]
  [#"^Clicks Gar.*$" :clicks [:medication]]
  [#"^I SnapScan Kne.*$" :knead [:food :eating-out :work-lunch]]
  [#"^'-387-6-1.*$" :savings [:save]]
  [#"^AIB       171000020.*$" :santam [:insurance]]
  [#"^NANDOS - BNDB.*$" :nandos [:food :eating-out]]
  [#"^T SMUTS.*$" :tanya-smuts [:psychology]]
  [#"^OUDEHOF APTEEK.*$" :pharmacy [:medication]]
  [#"^CHAMELEONS.*$" :chameleons [:school]]
  [#"^517374 WELGEMO.*$" :bp [:petrol]]
  [#"^TOTALSPORTS.*$" :totalsports [:clothes]]
  [#"^GELATO MANIA.*$" :gelato-mania [:food :eating-out]]
  [#"^HARBOUR HOUSE.*$" :harbour-house [:food :eating-out]]
  [#"^KENRIDGE 5838.*$" :atm [:cash]]
  ;[#"^.*$" : [:]]
  ;[#"^.*$" : [:]]
  ;[#"^.*$" : [:]]
  ;[#"^.*$" : [:]]
  ;[#"^.*$" : [:]]
  ;[#"^.*$" : [:]]
  ;[#"^.*$" : [:]]
  ;[#"^.*$" : [:]]
  ;[#"^.*$" : [:]]
  ;[#"^.*$" : [:]]
  ;[#"^.*$" : [:]]
  ;[#"^.*$" : [:]]
  ;[#"^.*$" : [:]]
  ;[#"^.*$" : [:]]
  ;[#"^.*$" : [:]]
])

(defn csv-data->maps [csv-data]
  (map zipmap
    (->> (first csv-data) ;; First row is the header
         (map keyword) ;; Drop if you want string keys instead
         repeat)
  	(rest csv-data)))

(defn first-match [csv-record]
  (first
    (filter
      (fn [category]
        (let [
          match (re-matches (first category) (:description csv-record))]
          match))
      categorization)))

(defn categorize [csv-data->maps]
  (map (fn [csv-record]
    (let [
        category (first-match csv-record)
        categorized {
            :supplier (second category)
            :tags (last category)
            :amount (Double/parseDouble (:amount csv-record))
            :record csv-record
          }]
      categorized)
      ) csv-data->maps))

(defn parse-all [filename]
  (with-open [reader (io/reader filename)]
    (doall (->>
      (csv/read-csv reader)
      csv-data->maps
      categorize))))

(defn calculate [tag categorized]
  (let [tagged (filter (fn [row] (some #(= tag %) (:tags row))) categorized)]
    (println tag (reduce + (map #(:amount %) tagged)))
  ))

(defn -main
  "Read and categorize expenses"
  [& args]
  (let [
    all (parse-all (first args))
    categorized (filter #(not (= nil (:supplier %))) all)
    uncategorized (filter #(= nil (:supplier %)) all)
    uncategorized-debits (filter #(>= 0 (:amount %)) uncategorized)
    ]
    (doseq [record (map #(:record %) uncategorized-debits)]
      (println (:description record) (:source record) (:amount record)))
    (println (count uncategorized-debits)
    (calculate :food categorized)
    (calculate :eating-out categorized)
    (calculate :cash categorized))))
