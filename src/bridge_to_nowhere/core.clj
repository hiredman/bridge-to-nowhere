(ns bridge-to-nowhere.core
  (:import [java.lang.management ManagementFactory]
           [javax.management ObjectName MBeanInfo MBeanAttributeInfo
            DynamicMBean AttributeList Attribute]
           [java.lang.reflect Proxy InvocationHandler]
           [clojure.lang Compiler]))

(defmulti invoker (fn [proxy method args atom-map] (keyword (.getName method))))

(defmethod invoker :setAttributes [_ _ [attr-list] atom-map]
  (doseq [att attr-list
          :let [k (keyword (.getName att))
                v (.getValue att)]]
    (swap! atom-map assoc k v)))

(defmethod invoker :setAttribute [_ _ [attr] atom-map]
  (let [a-key (.getName attr)
        a-value (.getValue attr)]
    (swap! atom-map assoc (keyword a-key) a-value)))

(defmethod invoker :getAttributes [_ _ [keys] atom-map]
  (let [a-map @atom-map
        a-list (AttributeList. (count a-map))]
    (doseq [[k v] a-map]
      (.add a-list (Attribute. (name k) v)))
    a-list))

(defmethod invoker :getAttribute [_ _ [a-key] atom-map]
  (get @atom-map (keyword a-key)))

(defn mbean-attribute-info [a-name a-class a-desc]
  (MBeanAttributeInfo. a-name a-class a-desc true true false))

(defn mbean-info [class-name a-desc attrs]
  (MBeanInfo. (.getName (class proxy))
              "Some Bean"
              attrs))

(defmethod invoker :getMBeanInfo [proxy _ _ atom-map]
  (let [attrs (into-array MBeanAttributeInfo
                          (for [[k v] @atom-map]
                            (mbean-attribute-info
                             (name k)
                             (.getName (or (and v (class v)) String))
                             (str "Property " (name k)))))]
    (mbean-info (.getName (class proxy))
                "Some Bean"
                attrs)))

(defmethod invoker :invoke [& _])

(defn make-bean [atom-map]
  (Proxy/newProxyInstance
   @Compiler/LOADER
   (into-array Class [DynamicMBean])
   (reify InvocationHandler
     (invoke [_ proxy method args]
       (invoker proxy method args atom-map)))))

(defn register [atom-map & {:keys [category name]}]
  (let [b (make-bean atom-map)]
    (.registerMBean
     (ManagementFactory/getPlatformMBeanServer)
     b
     (ObjectName. (str (or category *ns*)
                       ":type="
                       (or name (.getSimpleName (class b))))))))
