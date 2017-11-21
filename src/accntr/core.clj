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
  [#"^UBER SA [Hh][Ee][Ll][Pp].*$" :uber [:taxi]]
  [#"^MWEB IN   INTERNET.*$" :mweb [:internet]]
  [#"^272713422 VOD PREPA.*$" :vodacom [:phone]]
  [#"^VOD PREPAID.*$" :vodacom [:phone]]
  [#"^PAM GOLDING.*$" :pam-golding [:rent]]
  [#"^I SnapScan Ecc.*$" :ecco [:food :work-food]]
  [#"^SUPERSPAR WELG.*$" :spar [:food]]
  [#"^I SnapScan Del.*$" :deluxe [:food :coffee]]
  [#"^ABSOLUTE PETS.*$" :absolute-pets [:dog-food]]
  [#"^VOD PREPAID    0713369497.*$" :vodacom [:phone]]
  [#"^Clicks.*$" :clicks [:medical]]
  [#"^FIXED MONTHLY FEE 272713422.*$" :standard-bank [:bank-fees]]

  [#"^ACC   272713422.*$" :other-bank [:bank-fees]]
  [#"^ELEC           01023614165.*$" :eskom [:electricity]]
  [#"^DIS-CHEM [TYGER|TABS].*$" :dis-chem [:toiletries]]
  [#"^MONIQUE.*$" :monique [:baby-sitting]]
  [#"^LEANDRI.*$" :leandri [:baby-sitting]]
  [#"^WELGEMOED ANIM.*$" :welgemoed-anim [:dogs]]
  [#"^272713422 ELEC      01023614165.*$" :dis-chem [:toiletries]]
  [#"^OUMEUL WILABS.*$" :ou-meul [:food :eating-out]]
  [#"^PNP KENRIDGE.*$" :picknpay [:food]]
  [#"^Clicks Gar.*$" :clicks [:medical]]
  [#"^I SnapScan Kne.*$" :knead [:work-food]]
  [#"^I SnapScan Div.*$" :diva [:work-food]]
  [#"^'-387-6-1.*$" :savings [:save]]
  [#"^AIB.*$" :santam [:insurance]]
  [#"^NANDOS - BNDB.*$" :nandos [:food :eating-out]]
  [#"^T SMUTS.*$" :tanya-smuts [:psychology]]
  [#"^CLAIRE GREGOROWSKI.*$" :claire-gregorowski [:psychology]]
  [#"^OUDEHOF APTEEK.*$" :pharmacy [:medical]]
  [#"^CHAMELEONS.*$" :chameleons [:school]]
  [#"^517374 WELGEMO.*$" :bp [:petrol]]
  [#"^TOTALSPORTS.*$" :totalsports [:clothes]]
  [#"^GELATO MANIA.*$" :gelato-mania [:food :eating-out]]
  [#"^HARBOUR HOUSE.*$" :harbour-house [:food :eating-out]]
  [#"^KENRIDGE 5838.*$" :atm [:cash]]
  [#"^ABSA.*$" :atm [:cash]]

  [#"^KFC.*$" :kfc [:food :eating-out]]
  [#"^C*BP KENRIDGE.*$" :bp [:petrol]]
  [#"^Dogs Bollo.*$" :dogs-bollocks [:food :work-food]]
  [#"^BEN WEI SU.*$" :ben-wei [:food :work-food]]
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
  [#"^WELLNESS.*$" :wellness-center [:medical]]
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
  [#"^.*WATERFRON.*$" :waterfront [:other]]
  [#"^C\*Truworths.*$" :truworths [:clothes]]
  [#"^SBSA VAF.*$" :standard-bank [:cars]]
  [#"^UMI RESTAU.*$" :umi [:eating-out]]
  [#"^VIRGIN ACT.*$" :virgin-active [:exercise]]
  [#"^BILLABONG.*$" :billabong [:clothes]]
  [#"^MOTORPORT AUTO.*$" :bp [:petrol]]
  [#"^DIVA RESTA.*$" :diva [:work-food]]
  [#"^PnP Crp.*$" :picknpay [:food]]
  [#"^BONNIE.*$" :sam [:other]]
  [#"^KAUAI JUICE.*$" :kauai [:eating-out]]
  [#"^Truth Cof.*$" :truth [:coffee]]
  [#"^DR C TOMC.*$" :dr-christine-tomcheck [:medical]]
  [#"^PNP FAMILY.*$" :picknpay [:food]]
  [#"^PNP WILLOW.*$" :picknpay [:food]]
  [#"^STER KINE.*$" :ster-kinefor [:entertainment]]
  [#"^PATHCARE.*$" :pathcare [:medical]]
  [#"^TOY PLANET.*$" :toy-planet [:toys]]
  [#"^M-KEM MEDI.*$" :m-kem [:medical]]
  [#"^MELANIE BURGER.*$" :melanie-burger [:exercise]]
  [#"^ENGEN.*$" :engen [:petrol]]
  [#"^Save.*$" :save [:savings]]
  [#"^TYGERBERG VETS.*$" :tygerberg-vets [:dogs]]
  [#"^TABLES RESTAUR.*$" :tables [:eating-out]]
  [#"^COLCACCHIO.*$" :colcaccios [:eating-out]]
  [#"^COLCACHIO.*$" :colcaccios [:eating-out]]
  [#"^ZONNEBLOEM.*$" :bp [:petrol]]
  [#"^KNEAD.*$" :knead [:work-food]]
  [#"^HOUT BAY VET.*$" :hout-bay-vet [:dogs]]
  [#"^063643 WES.*$" :unknown [:other]]
  [#"^CARLUCCIS.*$" :carluccios [:eating-out]]
  [#"^CAFE ROUX.*$" :cafe-roux [:eating-out]]
  [#"^LEKKER.*$" :lekker [:eating-out]]
  [#"^CRAZY STORE.*$" :crazy-store [:other]]
  [#"^PNP CLOTHING.*$" :picknpay [:clothes]]
  [#"^PICK N PAY.*$" :picknpay [:food]]
  [#"^HARTLIEF.*$" :hartlief [:work-food]]
  [#"^SWEDO CARS.*$" :swedo-cars [:car-maintenance]]
  [#"^.*SWATCH.*$" :swatch [:clothes]]
  [#"^WLMD Mother an.*$" :mother [:other]]
  [#"^SEAT.*$" :seat-meet-eat [:eating-out]]
  [#"^QUAY 4.*$" :quay-4 [:eating-out]]
  [#"^JD V A.*$" :jd [:other]]
  [#"^I LOVE WAFFLES.*$" :i-love-waffles [:eating-out]]
  [#"^Deluxe Buitenk.*$" :deluxe [:coffee]]
  [#"^SUMMERHOUSE.*$" :summerhouse [:other]]
  [#"^WELGEMOE 6081.*$" :atm [:cash]]
  [#"^STOPFORTHS ATTORNEYS.*$" :stopforths [:ignore]]
  [#"^GAME.*$" :game [:other]]
  [#"^078723876001.*$" :transfer [:ignore]]
  [#"^KIM AND EMILY.*$" :kim-and-emily [:other]]
  [#"^Spar Welgedach.*$" :spar [:food]]
  [#"^KITE LAB.*$" :kite-lab [:kitesurfing]]
  [#"^NANDOS.*$" :nandos [:eating-out]]
  [#"^CHRIS WILLEMSE.*$" :cwc [:exercise]]
  [#"^MELTRADE.*$" :metrade [:entertainment]]
  [#"^THE HART.*$" :the-hart [:eating-out]]
  [#"^SPIROS.*$" :spiros [:other]]
  [#"^BOOTLEGGER.*$" :bootlegger [:eating-out]]
  [#"^DIAL 4 GAS.*$" :dial-4-gas [:electricity]]
  [#"^517374 WEL.*$" :bp [:petrol]]
  [#"^GSP RESCUE.*$" :gsp-rescue [:dogs]]
  [#"^LOOKOUT DEAUTO.*$" :bp [:petrol]]
  [#"^SUNNINGDALE CO.*$" :bp [:petrol]]
  [#"^PLATINUM HAIR.*$" :platinum-hair [:hair]]
  [#"^Gardens Centre.*$" :gardens-center [:parking]]
  [#"^Interpark.*$" :interpark [:parking]]
  [#"^ABOUT CATS.*$" :about-cats [:dogs]]
  [#"^WEST COAST.*$" :bp [:petrol]]
  [#"^TYGERVAL.*$" :atm [:cash]]
  [#"^(Spar St He|SPAR ST HE).*$" :spar [:food]]
  [#"^Spar Oakhurst.*$" :spar [:food]]
  [#"^Cltx Melkbos.*$" :caltex [:petrol]]
  [#"^.*FEE.*-.*OTHER BANK.*$" :other-bank [:bank-fees]]
  [#"^SACRED GROUND.*$" :sacred-ground [:eating-out]]
  [#"^VASA Tygervall.*$" :vasa-tygervalley [:unknown]]
  [#"^Builders Exp.*$" :builders-express [:other]]
  [#"^BELTHAZAR REST.*$" :belthazar [:eating-out]]
  [#"^MASSIMOS.*$" :massimos [:eating-out]]
  [#"^LIMNOS.*$" :limnos [:eating-out]]
  [#"^SANBI KIRSTEN.*$" :san-parks [:entertainment]]
  [#"^TMNP BOULDERS.*$" :san-parks [:entertainment]]
  [#"^DR M KUHN.*$" :dr-m-kuhn [:medical]]
  [#"^BABY BOOM WILL.*$" :baby-boom [:toys]]
  [#"^THE BLUE.*$" :the-blue [:eating-out]]
  [#"^NEW YORK.*$" :new-york [:eating-out]]
  [#"^MELONCINO.*$" :meloncino [:eating-out]]
  [#"^PNP TYGERVALLE.*$" :picknpay [:food]]
  [#"^WELGEDACHT TOP.*$" :tops [:food]]
  [#"^SORBET.*$" :sorbet [:eating-out]]
  [#"^.*SPUR.*$" :spur [:eating-out]]
  [#"^TYGERBERG ABS.*$" :absolute-pets [:dogs]]
  [#"^.*CACCHIO.*$" :colcaccios [:eating-out]]
  [#"^Spar Welge.*$" :spar [:food]]
  [#"^FOSSIL.*$" :fossil [:clothes]]
  [#"^HAAGEN DAZ.*$" :haagen-dazs [:eating-out]]
  [#"^ROYALE EAT.*$" :royale-eatery [:work-food]]
  [#"^CHARLYS B.*$" :charlys-bakery [:work-food]]
  [#"^SENTINEL SABS.*$" :bp [:petrol]]
  [#"^PET TRAVEL.*$" :pet-travel [:dogs]]
  [#"^OXFORD STAMLB .*$" :oxford-stam [:other]]
  [#"^CASSIA RESNDB .*$" :cassia [:eating-out]]
  [#"^FAMOUS K510000.*$" :famous [:eating-out]]
  [#"^MAINSTRE 0741 .*$" :atm [:cash]]
  [#"^STAR DOT TOYS .*$" :star-dot-toys [:toys]]
  [#"^Furniture     .*$" :furniture [:other]]
  [#"^CONSTANTIABERG.*$" :constantiaberg-mediclinic [:medical]]
  [#"^GUYS SERVICE C.*$" :guys-service [:other]]
  [#"^.*STEERS.*$" :steers [:eating-out]]
  [#"^KLOOF STRE1NB .*$" :kloof-street [:other]]
  [#"^DAMHUIS BECTB .*$" :damhuis [:eating-out]]
  [#"^DION WIRED TYG.*$" :dion-wired [:other]]
  [#"^CAFEEN    AUTO.*$" :cafeen [:eating-out]]
  [#"^PANCHO S  AUTO.*$" :pancho [:eating-out]]
  [#"^BASH VENUE    .*$" :bash-venue [:entertainment]]
  [#"^TYGER MA 5749 .*$" :atm [:cash]]
  [#"^BATTLE BUN1NB .*$" :battle-bunker [:entertainment]]
  [#"^JERRY\'S WIAUTO.*$" :jerrys-willowbrige [:eating-out]]
  [#"^THE ALPHEN LA .*$" :the-alphen [:eating-out]]
  [#"^ALLEGRIA CAFE .*$" :allegria [:eating-out]]
  [#"^LORRAINE PEDER.*$" :lorraine-pederson [:entertainment]]
  [#"^EAT AT ALTMLB .*$" :eat-at-alt [:eating-out]]
  [#"^.*THE GREEK ME.*$" :the-greek [:eating-out]]
  [#"^CROWN RESTABS .*$" :crown-restaurant [:eating-out]]
  [#"^BORRUSOS PABS .*$" :borrusos [:eating-out]]
  [#"^COPPERFIELD CH.*$" :copperfield [:other]]
  [#"^VOVO TELO NDB .*$" :vovo-telo [:eating-out]]
  [#"^.*SPORTSMANS W.*$" :sportsmans-warehouse [:exercise]]
  [#"^CNA TYGERVALLE.*$" :cna [:other]]
  [#"^.*CREATION WIN.*$" :creation-wines [:eating-out]]
  [#"^TWO OCEANSNDB .*$" :two-oceans [:eating-out]]
  [#"^.*YDE 0813    .*$" :yde [:clothes]]
  [#"^NEDCOR.*$" :atm [:cash]]
  [#"^PINE GARDE.*$" :pine-garden [:eating-out]]    
  [#"^MONT MARIE.*$" :mont-marie [:eating-out]]
  [#"^CORICRAFT.*$" :coricraft [:other]]
  [#"^KE-MONATE REST.*$" :ke-monate [:eating-out]]
  [#"^VODACOM SERVIC.*$" :vodacom [:phone]]
  [#"^DR J LA GRANGE.*$" :dr-j-la-grange [:medical]]
  [#"^SUPER SPAR AUR.*$" :spar [:food]]
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
  ;[#"^.*$" : [:]]
  ;[#"^.*$" : [:]]

])

(defn csv-data->maps [csv-data]
  (map zipmap
    (->> (first csv-data) ;; First row is the header
         (map keyword) ;; Drop if you want string keys instead
         repeat)
  	(rest csv-data)))

(defn first-match [csv-record tag]
  (first
    (filter
      (fn [category] (re-matches (first category) (tag csv-record)))
      categorization)))

(defn category-to-object [csv-record category] {
  :supplier (second category)
  :tags (last category)
  :amount (Double/parseDouble (:amount csv-record))
  :record csv-record})

(defn categorize [csv-data->maps]
  (map (fn [csv-record]
    (category-to-object csv-record
      (if-let [description-match (first-match csv-record :description)]
        description-match
        (if-let [source-match (first-match csv-record :source)]
            source-match))))
    csv-data->maps))

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
  (let [amount
      (reduce + (map
        (fn [c]
          (:amount c)) categorized))]
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
      (println (:description record) ": " (:source record) (:amount record) (:date record)))
    (println (count uncategorized-debits)
    (let [total
        (+
          (calculate "Rent" (search :rent categorized))
          (calculate "Cars" (search :cars categorized))
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
          (calculate "Car Maintenance" (search :car-maintenance categorized))
          (calculate "Parking" (search :parking categorized))
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
          (calculate "Medical" (search :medical categorized))
          (calculate "Psychology" (search :psychology categorized))
          (calculate "Toiletries" (search :toiletries categorized))
          (calculate "Babysitting" (search :baby-sitting categorized))
          (calculate "Bank Fees" (search :bank-fees categorized))
          (calculate "Internet" (search :internet categorized))
          (calculate "Insurance" (search :insurance categorized))
          (calculate "Coffee" (search-not :food (search :coffee categorized)))
          (calculate "Toys" (search :toys categorized))
          (calculate "Unknown" (search :unknown categorized))
          (calculate "Other" (search :other categorized))
          )
          ]
        (println total)
    ))))
