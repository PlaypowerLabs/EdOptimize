appCSS <- "
#loading-content {
position: absolute;
background: #000000;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}

.shiny-output-error-validation {
color: black;
background-color: white!important;
font-weight: bold;
font-size: 25px;
display: flex;
justify-content: center;
align-items: center;
padding-top: 5%;
width: 100%;
}

.bg-blue {
background-color: #415C76!important;
}
.bg-orange {
background-color: #ffb35c!important;
}
.bg-aqua {
background-color: #2EA3A3!important;
}
.box-header .box-title{
color: white;
}

table.dataTable thead th, table.dataTable thead td {
    padding: 10px 18px;
    border-bottom: 1px solid #ffffff;
    text-align: center;
    
}


.box {
    position: relative;
    border-radius: 3px;
    background: #ffffff;
    border-top: 3px solid #d2d6de;
    margin-bottom: 20px;
    width: 100%;
    box-shadow: 0 1px 1px rgba(0, 0, 0, 0.1);
}
.box.box-solid {
    border-top: 0;
}
.bg-blue {
background-color: #415C76!important;
}
.box.box-solid[class*='bg'] > .box-header {
    color: #ffffff;
}
.box-header {
    color: #444;
    display: block;
    padding: 10px;
    position: relative;
}
.box-body {
    border-top-left-radius: 0;
    border-top-right-radius: 0;
    border-bottom-right-radius: 3px;
    border-bottom-left-radius: 3px;
    padding: 10px;
}

.bg-teal {
    background-color: #39cccc!important;
}
.info-box {
    display: block;
    min-height: 90px;
    background: #fff;
    width: 100%;
    box-shadow: 0 1px 1px rgba(0,0,0,.1);
    border-radius: 2px;
    margin-bottom: 15px;
}
.info-box-icon {
    border-top-left-radius: 2px;
    border-top-right-radius: 0;
    border-bottom-right-radius: 0;
    border-bottom-left-radius: 2px;
    display: block;
    float: left;
    height: 90px;
    width: 90px;
    text-align: center;
    font-size: 45px;
    line-height: 90px;
    background: rgba(0,0,0,.2);
}
.fa, .fab, .fal, .far, .fas {
    -moz-osx-font-smoothing: grayscale;
    -webkit-font-smoothing: antialiased;
    display: inline-block;
    font-style: normal;
    font-variant: normal;
    text-rendering: auto;
    line-height: 1;
}
.fa, .fas {
    font-weight: 900;
}
.alert-danger, .alert-error, .alert-info, .alert-success, .alert-warning, .bg-aqua, .bg-aqua-active, .bg-black, .bg-black-active, .bg-blue, .bg-blue-active, .bg-fuchsia, .bg-fuchsia-active, .bg-green, .bg-green-active, .bg-light-blue, .bg-light-blue-active, .bg-lime, .bg-lime-active, .bg-maroon, .bg-maroon-active, .bg-navy, .bg-navy-active, .bg-olive, .bg-olive-active, .bg-orange, .bg-orange-active, .bg-purple, .bg-purple-active, .bg-red, .bg-red-active, .bg-teal, .bg-teal-active, .bg-yellow, .bg-yellow-active, .callout.callout-danger, .callout.callout-info, .callout.callout-success, .callout.callout-warning, .label-danger, .label-info, .label-primary, .label-success, .label-warning, .modal-danger .modal-body, .modal-danger .modal-footer, .modal-danger .modal-header, .modal-info .modal-body, .modal-info .modal-footer, .modal-info .modal-header, .modal-primary .modal-body, .modal-primary .modal-footer, .modal-primary .modal-header, .modal-success .modal-body, .modal-success .modal-footer, .modal-success .modal-header, .modal-warning .modal-body, .modal-warning .modal-footer, .modal-warning .modal-header {
    color: #fff!important;
}

.info-box-content {
    padding: 5px 10px;
    margin-left: 90px;
}
.info-box-text {
    font-size: 32px;
}
.info-box-text {
    text-transform: uppercase;
}
.info-box-number {
    display: block;
    font-weight: 700;
    font-size: 18px;
}
"