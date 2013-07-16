(defproject ttc-2013-flowgraphs "0.0.1"
  :description "A Solution for the TTC 2013 Flowgraphs Case."
  :url "https://github.com/tsdh/ttc-2013-flowgraphs"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [funnyqt "0.4.23"]]
  :license {:name "GNU General Public License, Version 3"
            :url "http://www.gnu.org/licenses/gpl.html"
            :distribution :repo}
  :warn-on-reflection true
  :jvm-opts ^:replace ["-Xms800m" "-Xmx800m"]
  :jar-exclusions [#"(?:^|/).(svn|hg|git|tg|tg\.gz|xmi|xmi\.gz)/"])
