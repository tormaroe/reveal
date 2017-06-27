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
        summary: {},
        room: {},
        includeAllAccessable: false,
        features: [],
        packages: [],
        selectedPackage: [],
        packageSymbols: [],
        selectedSymbol: [],
        symbolDescription: ''
      },
      methods: {
        loadSummary: function () {
          var that = this;
          this.$http.get('/data/summary').then(function (res) {
            that.summary = res.body;
          });
        },
        loadRoom: function () {
          var that = this;
          this.$http.get('/data/room').then(function (res) {
            that.room = res.body;
          });
        },
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
        loadSymbols: function () {
          var that = this;
          console.log('LOAD SYMBOLS');
          this.$http.get('/data/package-symbols?package=' + this.selectedPackage + '&all=' + this.includeAllAccessable).then(function (res) {
            that.packageSymbols = res.body;
          });
        }
      },
      watch: {
        selectedPackage: function (val) {
          console.log(val);
          console.log(this.selectedPackage);
          this.loadSymbols();
        },
        selectedSymbol: function (val) {
          var that = this;
          this.$http.get('/data/describe-symbol?package=' + this.selectedPackage + '&symbol=' + val).then(function (res) {
            that.symbolDescription = res.body;
          });
        },
        includeAllAccessable: function (val) {
          this.loadSymbols();
        }
      },
      mounted: function () {
        this.loadSummary();
        this.loadRoom();
        this.loadFeatures();
        this.loadAllPackages();
      },
    });
    " s))

(defun image-details-view (s)
  (with-html-output (s)
    (:div :class "row"
      (:table :class "table table-striped"
        (:tr (:td (:b (str "lisp-implementation-type")))
             (:td (str "{{ summary['lisp-implementation-type'] }}")))
        (:tr (:td (:b (str "lisp-implementation-version")))
             (:td (str "{{ summary['lisp-implementation-version'] }}")))
        (:tr (:td (:b (str "machine-instance")))
             (:td (str "{{ summary['machine-instance'] }}")))
        (:tr (:td (:b (str "machine-type")))
             (:td (str "{{ summary['machine-type'] }}")))
        (:tr (:td (:b (str "machine-version")))
             (:td (str "{{ summary['machine-version'] }}")))
        (:tr (:td (:b (str "software-type")))
             (:td (str "{{ summary['software-type'] }}")))
        (:tr (:td (:b (str "software-version")))
             (:td (str "{{ summary['software-version'] }}")))))
    (:div :class "row"

      (:pre 
        (:a :class "pull-right" :href "#" :|v-on:click| "loadRoom"
          (:span :class "glyphicon glyphicon-refresh" :aria-hidden "true"))
        (str "{{ room }}")))))

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
        (:h4 (str "Packages"))
        (packages-view s))
      (:div :class "col-md-6"
        (:h4 (str "Symbols in package ")
          (:small
            (:input :type "checkbox" :v-model "includeAllAccessable")
            (str "include all accessible")))
        (package-symbols-view s)))
    (:div :class "row"
      (:div :class "col-md-12"
        (:pre :style "margin-top: 10px;" 
          (str "{{ symbolDescription }}"))))))

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
                (:a :href "#image" :role "tab" :data-toggle "tab" (str "Image details")))
              (:li :role "presentation"
                (:a :href "#symbols" :role "tab" :data-toggle "tab" (str "Symbol browser")))
              (:li :role "presentation"
                (:a :href "#features" :role "tab" :data-toggle "tab" (str "Features"))))
            (:div :class "tab-content"
              (:div :role "tabpanel" :class "tab-pane active" :id "image" 
                (image-details-view s))
              (:div :role "tabpanel" :class "tab-pane" :id "symbols" 
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
