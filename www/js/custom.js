// Real-time clock script
function updateClock() {
    const clockElement = document.getElementById("dynamic-clock");
    if (clockElement) {
      const now = new Date();
      const timeString = now.toLocaleTimeString([], { 
        hour: '2-digit', 
        minute: '2-digit', 
        second: '2-digit' 
      });
      clockElement.textContent = timeString;
    }
  }
  
  // Update the clock every second
  setInterval(updateClock, 1000);


  $(document).on('blur', '.format-number', function() {
    var num = $(this).val().replace(/,/g, '');
    if (!isNaN(num) && num !== '') {
      $(this).val(Number(num).toLocaleString());
    }
  });


  // www/js/custom.js
Shiny.addCustomMessageHandler('scrollToElement', function(id) {
  var el = document.getElementById(id);
  if (el) {
    el.scrollIntoView({ behavior: 'smooth' });
  }
});


$(function () {
  // Hide ANY open popover when the click target is
  //   – NOT the icon itself ('.info-icon'), and
  //   – NOT inside an already-open popover ('.popover')
  $('body').on('click', function (e) {
    if (!$(e.target).closest('.popover, .info-icon').length) {
      $('.info-icon').popover('hide');
    }
  });
});
