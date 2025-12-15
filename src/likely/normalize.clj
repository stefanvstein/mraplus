(ns likely.normalize
  (:import [java.text Normalizer Normalizer$Form]
           [java.lang Character])
  (:require [clojure.set :as set]))

(defn protect-chars
  "Uppercase + byt ut skyddade tecken mot PUA-placeholders i en pass."
  ^String [^String s {:keys [orig->ph]}]
  (let [u (.toUpperCase s)
        n (.length u)
        sb (StringBuilder. n)]
    (loop [i 0]
      (if (= i n)
        (.toString sb)
        (let [ch (.charAt u i)]
          (.append sb (char (int (or (and orig->ph (orig->ph ch)) ch))))
          (recur (unchecked-inc i)))))))

(defn strip-combining
  "Ta bort combining marks (Mn/Mc/Me) i en pass."
  ^String [^String s]
  (let [n (.length s)
        sb (StringBuilder. n)]
    (loop [i 0]
      (if (= i n)
        (.toString sb)
        (let [ch (.charAt s i)
              t  (Character/getType ch)]
          (when (not (or (= t Character/NON_SPACING_MARK)
                         (= t Character/COMBINING_SPACING_MARK)
                         (= t Character/ENCLOSING_MARK)))
            (.append sb ch))
          (recur (unchecked-inc i)))))))

(defn  restore-and-filter
  "Återställ placeholders och filtrera till A–Z, skyddade och valfria keep-chars."
  ^String [^String s {:keys [ph->orig]} ^java.util.Set keep-chars]
  (let [n (.length s)
        sb (StringBuilder. n)]
    (loop [i 0]
      (if (= i n)
        (.toString sb)
        (let [ch  (.charAt s i)]
          (when-let [ch' (or (when (<= (int \A) (int ch) (int \Z)) ch)
                             (and keep-chars (keep-chars ch))
                             (and ph->orig (ph->orig ch)))]
            (.append sb ch'))
          (recur (unchecked-inc i)))))))

(defn ascii-only? [^CharSequence s]
  (let [n (.length s)]
    (loop [i 0]
      (if (= i n) true
          (let [c (.charAt s i)]
            (if (<= (int c) 0x7F)
              (recur (unchecked-inc i))
              false))))))

(defn normalize-ascii
  "Uppercase och filtrera A–Z + keep-chars."
  [^String s ^java.util.Set keep-chars]
  (let [u (.toUpperCase s)
        n (.length u)
        sb (StringBuilder. n)]
    (loop [i 0]
      (if (= i n)
        (.toString sb)
        (let [ch (.charAt u i)]
          (when (or (<= (int \A) (int ch) (int \Z))
                    (and keep-chars (keep-chars ch)))
            (.append sb ch))
          (recur (unchecked-inc i)))))))


;; ========== 3) Publik normaliserare ==========

(defn compile-keep-map
  "Creats a char to pua translation, as a precompiled for normalize"
  [protected-chars]
  (when protected-chars
    (if (string? protected-chars)
      (compile-keep-map (seq protected-chars))
      (let [prot-up (into #{} (map #(Character/toUpperCase ^char %)) protected-chars)
            pua-chars (map (fn [i] (char (+ 0xE000 i))) (range (count prot-up)))
            orig->ph  (zipmap prot-up pua-chars)
            ph->orig  (set/map-invert orig->ph)]
        {:orig->ph orig->ph
         :ph->orig ph->orig}))))

(defn  normalize
  "Normalizes a string. e.g. removing diacritics from letters, turning it incloser to uppercase ASCII letters. It can however be configured to keep certain diacritics, allowing for e.q. swedish with ÅÄÖ, by using keep-map. Some non letter ASCII can also be kept using keep-chars. The process of keeping diatrics will use PUA character set, so PUA is not recomended as input"
  ([s]
      (normalize s nil nil))
  ([^String s ph-maps]
   (normalize s ph-maps nil))
  ([^String s ph-maps keep-chars]
   (if (and ph-maps (or (string? ph-maps)
                        (sequential? ph-maps)))
     (normalize s (compile-keep-map ph-maps) keep-chars)
     (if  (ascii-only? s)

       (normalize-ascii s keep-chars)
       (let [p  (protect-chars s ph-maps)
             d  (Normalizer/normalize p Normalizer$Form/NFD)
             nm (strip-combining d)]
         (restore-and-filter nm ph-maps keep-chars))))))

(comment
;; välj vad som ska skyddas – utöka fritt (t.ex. även \Æ \Ø om du vill)
(def ph-maps (compile-keep-map [\Å \Ä \Ö \Æ \Ø]))
(compile-keep-map "ÅÄ")
;; sedan i pipen:
(normalize "Håkan  Björn – déjà vu" "åäö")
;; => "HÅKAN BJÖRN DEJA VU"
(normalize "Håkan  Björn – déjà vu")


(normalize "Allan sladd" )

(normalize "Jørgén på Ægir" ph-maps #{\space})
(normalize "Jørgén på Ægir" nil nil)
;; => "JØRGEN PÅ ÆGIR"

(normalize "Göte-borg's bästa" ph-maps #{\space \- \'})
;; => "GÖTE-BORG'S BÄSTA"
)
