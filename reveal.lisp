(in-package #:reveal)

(defparameter *default-port* 8591)

(defvar *app*)



(defun client-script (s)
  (princ "
    $('#myTabs a').click(function (e) {
      e.preventDefault()
      $(this).tab('show')
    });

    var revealApp = new Vue({
      el: '#revealApp',
      data: {
        features: [],
        packages: [],
        selectedPackage: [],
        packageSymbols: [],
        selectedSymbol: [],
        symbolDescription: ''
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
        }
      },
      watch: {
        selectedPackage: function (val) {
          var that = this;
          this.$http.get('/data/package-symbols?package=' + val).then(function (res) {
            that.packageSymbols = res.body;
          });
        },
        selectedSymbol: function (val) {
          var that = this;
          this.$http.get('/data/describe-symbol?package=' + this.selectedPackage + '&symbol=' + val).then(function (res) {
            that.symbolDescription = res.body;
          });
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
      (:select :class "form-control" :size "10" :v-model "selectedPackage"
        (:option :v-for "p in packages"
          (str "{{ p }}"))))))

(defun package-symbols-view (s)
  (with-html-output (s)
    (:div
      (:select :class "form-control" :size "10" :v-model "selectedSymbol"
        (:option :v-for "s in packageSymbols"
          (str "{{ s }}"))))))

(defun package-browser (s)
  (with-html-output (s)
    (:div :class "row"
      (:div :class "col-md-6"
        (packages-view s))
      (:div :class "col-md-6"
        (package-symbols-view s)))
    (:div :class "row"
      (:div :class "col-md-12"
        (:pre (str "{{ symbolDescription }}"))))))

(define-easy-handler (index :uri "/") ()
  (with-html-output-to-string (s)
    (princ "<!DOCTYPE html>" s)
    (terpri s)
    (:html :lang "en"
      (:head (:title "Reveal")
        (:meta :name "viewport" :content "width=device-width, initial-scale=1")
        (:link :rel "stylesheet" :href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"))
      (:body
        (:div :id "revealApp" :class "container"
          (:h1 (str "Reveal"))
          (:div 
            (:ul :id "myTabs" :class "nav nav-tabs" :role "tablist"
              (:li :role "presentation" :class "active"
                (:a :href "#symbols" :role "tab" :data-toggle "tab" (str "Symbol browser")))
              (:li :role "presentation"
                (:a :href "#features" :role "tab" :data-toggle "tab" (str "Features"))))
            (:div :class "tab-content"
              (:div :role "tabpanel" :class "tab-pane active" :id "symbols" 
                (package-browser s))
              (:div :role "tabpanel" :class "tab-pane" :id "features" 
                (features-view s))))
          (:script :src "https://code.jquery.com/jquery-3.2.1.min.js")
          (:script :src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js")
          (:script :src "https://cdnjs.cloudflare.com/ajax/libs/vue/2.3.4/vue.min.js")
          (:script :src "https://cdn.jsdelivr.net/vue.resource/1.3.1/vue-resource.min.js")
          (:script :type "text/javascript" (client-script s)))))))

(defun start (&optional port)
  (let ((port (or port *default-port*))))
    (setf *app* (make-instance 'easy-acceptor :port port))
    (hunchentoot:start *app*)
    (format nil "~&Everything will be revealed at port ~a.~%" port)
    nil)

(defun stop ()
  (hunchentoot:stop *app*))
