(ns com.manigfeald.base85-test
  (:require [clojure.test :refer :all]
            [com.manigfeald.base85 :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop])
  (:import (java.nio ByteBuffer)))

(def test-input
  "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.")

(def test-encoded
  "9jqo^BlbD-BleB1DJ+*+F(f,q/0JhKF<GL>Cj@.4Gp$d7F!,L7@<6@)/0JDEF<G%<+EV:2F!,O<DJ+*.@<*K0@<6L(Df-\\0Ec5e;DffZ(EZee.Bl.9pF\"AGXBPCsi+DGm>@3BB/F*&OCAfu2/AKYi(DIb:@FD,*)+C]U=@3BN#EcYf8ATD3s@q?d$AftVqCh[NqF<G:8+EV:.+Cf>-FD5W8ARlolDIal(DId<j@<?3r@:F%a+D58'ATD4$Bl@l3De:,-DJs`8ARoFb/0JMK@qB4^F!,R<AKZ&-DfTqBG%G>uD.RTpAKYo'+CT/5+Cei#DII?(E,9)oF*2M7/c")

(deftest t-encode
  (let [es (encode (.getBytes test-input "utf-8"))
        n (max (count es) (count test-encoded))]
    (dotimes [i n]
      (is (= (.charAt es i) (.charAt test-encoded i))
          [i (.charAt es i) (.charAt test-encoded i)])))
  (is (= "" (encode (byte-array 0))))
  (is (= "!<" (encode (into-array Byte/TYPE [1]))))
  (is (= "!!" (encode (into-array Byte/TYPE [0]))))
  (is (= "!!!" (encode (into-array Byte/TYPE [0 0]))))
  (is (= "r;" (encode (into-array Byte/TYPE [-3])))))

(deftest t-decode
  (let [es (String. (decode test-encoded))
        n (max (count es) (count test-input))]
    (dotimes [i n]
      (is (= (.charAt es i) (.charAt test-input i))
          [i (.charAt es i) (.charAt test-input i)])))
  (is (= (vec (decode "")) []))
  (is (= (vec (decode "!!!")) [0 0])))

(deftest t-roundtrip
  (is (= [0] (vec (decode (encode (into-array Byte/TYPE [0]))))))
  (is (= (ByteBuffer/wrap (into-array Byte/TYPE [0]))
         (-> (into-array Byte/TYPE [0]) encode decode
             ByteBuffer/wrap)))
  (is (= [-1] (vec (decode (encode (into-array Byte/TYPE [-1]))))))
  (is (= [-2] (vec (decode (encode (into-array Byte/TYPE [-2]))))))
  (is (= [-3] (vec (decode (encode (into-array Byte/TYPE [-3]))))))
  (is (= [0 0] (vec (decode (encode (into-array Byte/TYPE [0 0]))))))
  )

(defspec random-byte-arrays
  1000
  (prop/for-all [ba (gen/vector gen/byte)]
                (let [ba (into-array Byte/TYPE ba)
                      a (ByteBuffer/wrap ba)
                      b (-> ba encode decode (ByteBuffer/wrap))]
                  (= a b))))
