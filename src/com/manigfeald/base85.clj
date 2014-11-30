(ns com.manigfeald.base85
  (:import (java.nio.channels WritableByteChannel
                              ReadableByteChannel
                              Channel
                              Channels
                              ClosedChannelException)
           (java.nio ByteBuffer)
           (java.io ByteArrayInputStream
                    ByteArrayOutputStream)))

(defn base85-buffer-transform
  [^ByteBuffer ibuffer ^ByteBuffer obuffer end?]
  (let [r (- (.limit ibuffer)
             (inc (.position ibuffer)))]
    (while (.hasRemaining ibuffer)
      (.put ibuffer (byte 0)))
    (.flip ibuffer)
    (let [n (bit-and (long (.getInt ibuffer)) 0xffffffff)]
      (assert (not (neg? n)))
      (loop [i 5
             n n]
        (when (> i 0)
          (let [q  (quot n 85)
                rr (rem n 85)]
            (.put obuffer (dec i) (byte (+ rr 33)))
            (recur (dec i) q)))))
    (.limit obuffer (- (.capacity ibuffer) r))))

(defn buffered-write-channel-transform
  [xform ibuffer-size obuffer-size]
  (fn [^WritableByteChannel output-channel & [close?]]
    (let [bb (ByteBuffer/allocate ibuffer-size)
          out (ByteBuffer/allocate obuffer-size)
          open? (atom true)
          lock (Object.)]
      (reify
        Channel
        (isOpen [_]
          @open?)
        (close [_]
          (when @open?
            (when (pos? (.position bb))
              (locking lock
                (xform bb out true)
                (while (.hasRemaining out)
                  (.write output-channel out))))
            (reset! open? false))
          (when close?
            (.close output-channel)))
        WritableByteChannel
        (write [_ src-bb]
          (when-not (or @open? (not (.isOpen output-channel)))
            (throw (ClosedChannelException.)))
          (locking lock
            (loop [n 0]
              (if-not (.hasRemaining src-bb)
                n
                (do
                  (.put bb (.get src-bb))
                  (when (not (.hasRemaining bb))
                    (xform bb out false)
                    (while (.hasRemaining out)
                      (.write output-channel out))
                    (.rewind out)
                    (.rewind bb))
                  (recur (inc n)))))))))))

(def base85-write-channel
  (buffered-write-channel-transform base85-buffer-transform 4 5))

(defn encode [^bytes bytes]
  (with-open [baos (java.io.ByteArrayOutputStream.)
              c (Channels/newChannel baos)
              ^WritableByteChannel b (base85-write-channel c)]
    (.write b (java.nio.ByteBuffer/wrap bytes))
    (.close b)
    (String. (.toByteArray baos))))

(defn base85-read-buffer-transform
  [^ByteBuffer raw-bytes ^ByteBuffer transformed-bytes end?]
  (let [r (- (.limit raw-bytes)
             (dec (.position raw-bytes)))]
    (while (.hasRemaining raw-bytes)
      (.put raw-bytes (byte \u)))
    (.flip raw-bytes)
    (loop [i 0
           n 0]
      (if (> 5 i)
        (let [w (.get raw-bytes)
              x (- w 33)
              q (+ x (* n 85))]
          (recur (inc i) q))
        (do
          (.putInt transformed-bytes (unchecked-int n))
          (.rewind transformed-bytes))))
    (.limit transformed-bytes (max 0 (- (.capacity raw-bytes) r)))))

(defn buffered-read-channel-transform
  [xform ibuffer-size obuffer-size]
  (fn [^ReadableByteChannel input-channel & [close?]]
    (let [raw-bytes (ByteBuffer/allocate ibuffer-size)
          transformed-bytes (doto (ByteBuffer/allocate obuffer-size)
                              (.position obuffer-size))
          open? (atom true)
          done? (atom false)
          lock (Object.)]
      (reify
        Channel
        (isOpen [_]
          @open?)
        (close [_]
          (reset! open? false)
          (when close?
            (.close input-channel)))
        ReadableByteChannel
        (read [_ dest-bb]
          (when-not (or @open? (not (.isOpen input-channel)))
            (throw (ClosedChannelException.)))
          (if @done?
            -1
            (locking lock
              (loop [n 0]
                (if (or (not (.hasRemaining dest-bb))
                        (and @done?
                             (not (.hasRemaining transformed-bytes))))
                  n
                  (do
                    (when-not (.hasRemaining transformed-bytes)
                      (.rewind transformed-bytes)
                      (.rewind raw-bytes)
                      (loop []
                        (if (.hasRemaining raw-bytes)
                          (if (neg? (.read input-channel raw-bytes))
                            (do
                              (reset! done? true)
                              (xform raw-bytes transformed-bytes true))
                            (recur))
                          (xform raw-bytes transformed-bytes false))))
                    (if (.hasRemaining transformed-bytes)
                      (do
                        (.put dest-bb (.get transformed-bytes))
                        (recur (inc n)))
                      (recur n))))))))))))

(def base85-read-channel
  (buffered-read-channel-transform base85-read-buffer-transform 5 4))

(defn decode [^String str]
  (let [bytes (.getBytes str "utf-8")]
    (with-open [bais (ByteArrayInputStream. bytes)
                c (Channels/newChannel bais)
                ^ReadableByteChannel b (base85-read-channel c)
                baos (ByteArrayOutputStream.)]
      (let [buf (ByteBuffer/allocate 1024)]
        (while (not (neg? (.read b buf)))
          (.write baos (.array buf) 0 (.position buf)))
        (.toByteArray baos)))))
