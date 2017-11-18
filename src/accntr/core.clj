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
  [#"^I SnapScan Ecc.*$" :ecco [:food :work-food]]
  [#"^SUPERSPAR WELG.*$" :spar [:food]]
  [#"^I SnapScan Del.*$" :deluxe [:food :coffee]]
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
  [#"^OUMEUL WILABS.*$" :ou-meul [:food :eating-out]]
  [#"^PNP KENRIDGE.*$" :picknpay [:food]]
  [#"^Clicks Gar.*$" :clicks [:medication]]
  [#"^I SnapScan Kne.*$" :knead [:food :work-food]]
  [#"^'-387-6-1.*$" :savings [:save]]
  [#"^AIB.*$" :santam [:insurance]]
  [#"^NANDOS - BNDB.*$" :nandos [:food :eating-out]]
  [#"^T SMUTS.*$" :tanya-smuts [:psychology]]
  [#"^CLAIRE GREGOROWSKI.*$" :claire-gregorowski [:psychology]]
  [#"^OUDEHOF APTEEK.*$" :pharmacy [:medication]]
  [#"^CHAMELEONS.*$" :chameleons [:school]]
  [#"^517374 WELGEMO.*$" :bp [:petrol]]
  [#"^TOTALSPORTS.*$" :totalsports [:clothes]]
  [#"^GELATO MANIA.*$" :gelato-mania [:food :eating-out]]
  [#"^HARBOUR HOUSE.*$" :harbour-house [:food :eating-out]]
  [#"^KENRIDGE 5838.*$" :atm [:cash]]
  [#"^KFC.*$" :kfc [:food :eating-out]]
  [#"^C*BP KENRIDGE.*$" :bp [:petrol]]
  [#"^Dogs Bollo.*$" :dogs-bollocks [:food :work-food]]
  [#"^BEN WEI SUSHI.*$" :ben-wei [:food :work-food]]
  [#"^.*WOLFPACK RUN.*$" :run-specialist-store [:exercise]]
  [#"^NIKE V&A CAPE.*$" :nike [:exercise]]
  [#"^SIMPLY ASIA.*$" :simply-asia [:food :eating-out]]
  [#"^PRIMI PIATTI.*$" :primi-piatti [:food :eating-out]]
  [#"^TIMO.*$" :timo [:kitesurfing :lessons]]
  [#"^CABRINHA KITEBOARD.*$" :cabrinha [:kitesurfing]]
  [#"^Vineyard Deli.*$" :vineyard-deli [:food]]
  [#"^.*BP KENRIDG.*$" :bp [:petrol]]
  [#"^SUNGLASS HUT.*$" :sunglass-hut [:clothes]]
  [#"^WOODHEADS.*$" :woodheads [:clothes]]
  [#"^VILLAGE SERVIC.*$" :village-service [:petrol]]
  [#"^BLOEMENDAL WIN.*$" :bon-amis [:food :eating-out]]
  [#"^BON AMIS.*$" :bon-amis [:food :eating-out]]
  [#"^PAUL GRAPENDAAL.*$" :paul [:other]]
  [#"^COTTON ON KIDS.*$" :cotton-on-kids [:clothes]]
  [#"^.*CAFE ROUSSE.*$" :cafe-rousse [:food :eating-out]]
  [#"^WELLNESS.*$" :wellness-center [:medication]]
  [#"^PEARLYS.*$" :pearlys [:food :eating-out]]
  [#"^DISC PREM.*$" :discovery [:exercise]]
  [#"^.*SCRATCH PATC.*$" :scratch-patch [:entertainment]]
  [#"^MUSICAL RASCALS.*$" :musical-rascals [:school]]
  [#"^FRANK FOWDEN.*$" :frank-fowden [:hair]]
  [#"^YANCKE BAXTER.*$" :yanke-baxter [:school]]
  [#"^THE BALLET BOX.*$" :the-ballet-box [:exercise :ballet]]
  [#"^JET TYGERVALLE.*$" :jet [:clothes]]
  [#"^MARCELS.*$" :marcels [:food :eating-out]]
  [#"^SFH COSMIC CA.*$" :cosmic-candy [:food :eating-out]]
  [#"^THE COCK N BUL.*$" :cock-n-bull [:other]]
  [#"^SHOP 9 CONSTAN.*$" :toy-shop [:toys]]
  [#"^TASHA'S.*$" :tashas [:food :eating-out]]
  [#"^DURBANVILLE GA.*$" :durbanville-games [:entertainment]]
  [#"^PnP Crp Consta.*$" :picknpay [:food]]
  [#"^SUPERDRY.*$" :superdry [:clothes]]
  [#"^Musica.*$" :musica [:entertainment]]
  [#"^.*Snap[Ss]can.*$" :snapscan [:snapscan]]
  [#"^THE WATERFRONT.*$" :waterfront [:other]]
  [#"^C\*Truworths.*$" :truworths [:clothes]]
  [#"^SBSA VAF.*$" :standard-bank [:cars]]
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

(defn search [tag categorized]
  (filter (fn [row] (some #(= tag %) (:tags row))) categorized))

(defn search-not [tag categorized]
  (filter (complement (fn [row] (some #(= tag %) (:tags row)))) categorized))

(defn calculate [description categorized]
  (let [amount (reduce + (map #(:amount %) categorized))]
    (println description amount)
    amount))

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
    (let [total
        (+
          (calculate "Rent" (search :rent categorized))
          (calculate "Cars" (search :cars categorized))
          ;(calculate "Save" (search :save categorized))
          (calculate "Food"
            (search-not :coffee
              (search-not :work-food
                (search-not :eating-out
                  (search :food categorized)))))
          (calculate "Eating out" (search :eating-out categorized))
          (calculate "Work food" (search :work-food categorized))
          (calculate "Cash" (search :cash categorized))
          (calculate "Dogs" (search :dogs categorized))
          (calculate "Taxi" (search :taxi categorized))
          (calculate "Petrol" (search :petrol categorized))
          (calculate "Exercise" (search :exercise categorized))
          (calculate "Clothes" (search :clothes categorized))
          (calculate "Kitesurfing" (search :kitesurfing categorized))
          (calculate "Entertainment" (search :entertainment categorized))
          (calculate "SnapScan" (search :snapscan categorized))
          (calculate "Phone" (search :phone categorized))
          (calculate "Dogfood" (search :dog-food categorized))
          (calculate "Electricity" (search :electricity categorized))
          (calculate "Toys" (search :toys categorized))
          (calculate "School" (search :school categorized))
          (calculate "Hair" (search :hair categorized))
          (calculate "Medication" (search :medication categorized))
          (calculate "Psychology" (search :psychology categorized))
          (calculate "Toiletries" (search :toiletries categorized))
          (calculate "Babysitting" (search :baby-sitting categorized))
          (calculate "Bank Fees" (search :bank-fees categorized))
          (calculate "Internet" (search :internet categorized))
          (calculate "Insurance" (search :insurance categorized))
          (calculate "Coffee" (search :coffee categorized))
          (calculate "Other" (search :other categorized)))]
        (println total)
    ))))
