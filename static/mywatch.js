$(function() {
  var info = $('#info');
  var infoHead = $('#info>h1');
  var infoAlert = $('#info>div');
  var main = $('#main');
  var plBody = $('#processList>tbody');

  var interval = null;

  var plCols = $('#processList>thead>tr>th')
    .map(function() {
      return $(this).text();
    }).get();


  function commonError(jqXHR, textStatus, errorThrown) {
    plBody.empty();
    main.hide();
    infoHead.text('An error has occured');
    infoAlert.text((0 == jqXHR.readyState) ? 'Service unavailable' : errorThrown);
    infoAlert.removeClass().addClass('alert alert-danger');
    info.show();
  }

  function getProcessList(server) {
    $.ajax({
      url: "server/" + server + "/processlist.json",
      method: "GET",
      error: commonError,
      success: function(procs) {
        plBody.empty();
        procs.map(function(p) {
          var tr = $('<tr>');
          plCols.map(function(c) {
            var td = $('<td>');
            td.text(p[c]);
            if ('info' === c) {
              td.css('white-space', 'pre-wrap');
            } else if ('time' === c) {
              td.css('text-align', 'right');
            } else if ('id' === c) {
              td.css('text-align', 'right');
            }
            tr.append(td);
          });
          plBody.append(tr);
        });
        info.hide();
        main.show();
      }
    });
  };

  $.ajax({
    url: "serverlist.json",
    method: "GET",
    error: commonError,
    success: function(servers) {
      $.each(servers, function(i, s) {
        $('#serverList>ul')
          .append('<li><a href="#">' + s +
            '</a></li>')
      });
      $("#serverList a").on("click", function() {
        var server = $(this).text();
        $(this).parent().parent().find('.active').removeClass('active');
        $(this).parent().addClass('active');
        clearInterval(interval);
        getProcessList(server);
        interval = setInterval(getProcessList, 60 * 1000, server);
      });
      info.hide();
    }
  });
});
