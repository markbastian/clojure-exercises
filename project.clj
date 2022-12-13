(defproject clojure-exercises "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [com.taoensso/tufte "2.0.1"]
                 [clj-http/clj-http "3.12.3"]
                 [cheshire/cheshire "5.10.0"]]

  :source-paths ["src/main/clj" "src/main/cljc"]
  :test-paths ["src/test/clj" "src/test/cljc"]
  :resource-paths ["src/main/resources"]
  :java-source-paths ["src/main/java"])
