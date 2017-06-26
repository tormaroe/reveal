(in-package #:reveal)

(defparameter *default-port* 8591)

(defvar *app*)



(defun client-script (s)
  (princ "
    var revealApp = new Vue({
      el: '#revealApp',
      data: {
        features: [],
        packages: [],
        selectedPackage: [],
        packageSymbols: []
      },
      methods: {
        loadFeatures: function () {
          var that = this;
          this.$http.get('/data/features').then(function (res) {
            that.features = res.body;
          });
        },
        loadAllPackages: function () {
          var that = this;
          this.$http.get('/data/all-packages').then(function (res) {
            that.packages = res.body;
          });
        },
        loadPackageSymbols: function (p) {
          var that = this;
          console.log('Package selected: ' + p);
          this.$http.get('/data/package-symbols?package=' + p).then(function (res) {
            console.dir(res);
            that.packageSymbols = res.body;
          });
        }
      },
      watch: {
        selectedPackage: function (val) {
          this.loadPackageSymbols(val);
        }
      },
      mounted: function () {
        this.loadFeatures();
        this.loadAllPackages();
      },
    });
    " s))

(defun features-view (s)
  (with-html-output (s)
    (:div
      (:select :size "10"
        (:option :v-for "f in features"
          (str "{{ f }}"))))))

(defun packages-view (s)
  (with-html-output (s)
    (:div
      (:select :size "10" :v-model "selectedPackage" ;:v-on "change: loadPackageSymbols"
        (:option :v-for "p in packages"
          (str "{{ p }}")))
      (str "{{ selectedPackage }}"))))

(defun package-symbols-view (s)
  (with-html-output (s)
    (:div
      (:select :size "10"
        (:option :v-for "s in packageSymbols"
          (str "{{ s }}"))))))

(define-easy-handler (index :uri "/") ()
  (with-html-output-to-string (s)
    (:html
      (:head (:title "Reveal"))
      (:body
        (:div :id "revealApp"
          (:h1 (str "Reveal"))
          (packages-view s)
          (package-symbols-view s)
          (features-view s)
          ;(:script :src "https://code.jquery.com/jquery-3.2.1.min.js")
          (:script :src "https://cdnjs.cloudflare.com/ajax/libs/vue/2.3.4/vue.min.js")
          (:script :src "https://cdn.jsdelivr.net/vue.resource/1.3.1/vue-resource.min.js")
          (:script :type "text/javascript" (client-script s)))))))

(defun start (&optional port)
  (setf *app* (make-instance 'easy-acceptor 
                             :port (or port *default-port*)))
  (hunchentoot:start *app*))

(defun stop ()
  (hunchentoot:stop *app*))
