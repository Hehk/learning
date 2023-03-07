(defrecord Mushroom [color height])

(def regular-mushroom (Mushroom. "brown" 2))

(.-color regular-mushroom)

(defprotocol Edible
  (bite-right-side [this])
  (bite-left-side [this]))

(defrecord WonderlandMushroom [color height]
  Edible
  (bite-right-side [this]
    (str "The " color " bite makes you grow taller."))
  (bite-left-side [this]
    (str "The " color " bite makes you shrink.")))

(def wonderland-mushroom (WonderlandMushroom. "red" 2))

(bite-right-side wonderland-mushroom)

