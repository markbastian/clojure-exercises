(ns misc.http-client
  (:require [clj-http.client :as client]))

(comment
  (->> {:method :get
        :url    "https://icanhazdadjoke.com/"
        :accept :application/json
        :as     :json}
       client/request
       :body
       :joke))