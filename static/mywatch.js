$(function() {
  var info = $('#info');
  var infoAlert = $('#info>div');
  var infoHead = $('#info>h1');
  var main = $('#main');
  var plBody = $('#processList>tbody');
  var serverList = $('#serverList>ul');

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

  function switchServer(server) {
    clearInterval(interval);
    if ('' !== server) {
      document.title = server + ' — ' + 'MyWatch';
      serverList.find('.active').removeClass('active');
      var s = $('a[href="#' + server + '"]');
      if (s) {
        s.parent().addClass('active');
        getProcessList(server);
        interval = setInterval(getProcessList, 60 * 1000, server);
      }
    } else {
      document.title = 'MyWatch';
    }
  }

  function onHash() {
    switchServer(location.hash.substring(1));
  };
  window.onhashchange = onHash;

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
              td.addClass('mywatch-query');
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
    url: 'serverlist.json',
    method: 'GET',
    error: commonError,
    success: function(servers) {
      var total = servers.length;
      var available = [];
      var checked = 0;
      $.each(servers, function(i, s) {
        $.ajax({
          url: 'server/' + s + '/processlist.json',
          method: 'HEAD',
          success: function() {
            available.push(s);
          },
          complete: function() {
            checked++;
            if (checked === total) {
              $.each(available.sort(), function(i, s) {
                serverList.append('<li><a href="#' + s + '">' + s + '</a></li>')
              });
              serverList.find('a').on('click', function() {
                var s = $(this).text();
                if ('#' + s === location.hash) {
                  getProcessList(s);
                }
              });
              info.hide();
              onHash();
            }
          }
        });
      });
    }
  });
});