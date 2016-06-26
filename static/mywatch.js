$(function() {
  var info = $('#info');
  var infoAlert = $('#info>div');
  var infoHead = $('#info>h1');
  var main = $('#main');
  var plBody = $('#processList>tbody');
  var plHeader = $('#processList>thead>tr');
  var serverList = $('#serverList>ul');

  var cankill;
  var interval;
  var server;

  var plCols = plHeader.children().map(function() {
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

  function switchServer() {
    cankill = undefined;
    clearInterval(interval);
    if ('' !== server) {
      document.title = server + ' — ' + 'MyWatch';
      serverList.find('.active').removeClass('active');
      var s = $('a[href="#' + server + '"]');
      if (s) {
        s.parent().addClass('active');
        getProcessList();
        interval = setInterval(getProcessList, 60 * 1000);
      }
    } else {
      document.title = 'MyWatch';
    }
  }

  function onHash() {
    server = location.hash.substring(1);
    switchServer();
  };
  window.onhashchange = onHash;

  function kill(id) {
    $.ajax({
      url: 'server/' + server + '/process/' + id,
      method: 'DELETE',
      success: function() {
        $('#' + id).fadeOut(300, function() {
          $(this).remove();
        });
      }
    });
  }

  function showProcessList(procs) {
    plBody.empty();
    if (cankill) {
      if (!plHeader.children('#kill').length) {
        plHeader.prepend('<th id="kill">');
      }
    } else {
      plHeader.children('#kill').remove();
    }
    procs.map(function(p) {
      var tr = $('<tr id="' + p['id'] + '">');
      if (cankill) {
        var td;
        if (('' != p['host']) && (0 < p['time']) && ('Killed' != p['state'])) {
          td = $('<td role="button" title="KILL" class="btn btn-danger btn-xs">&nbsp;</td>');
          td.on('click', function() {
            kill($(this).parent().attr('id'));
          });
        } else {
          td = $('<td>');
        }
        tr.append(td);
      }
      plCols.map(function(c) {
        var td = $('<td>');
        if ('id' === c) {
          td.addClass('mywatch-number');
        } else if ('info' === c) {
          td.addClass('mywatch-query');
        } else if ('time' === c) {
          td.addClass('mywatch-number');
        }
        td.text(p[c]);
        tr.append(td);
      });
      plBody.append(tr);
    });
    info.hide();
    main.show();
  }

  function getProcessList() {
    function get() {
      $.ajax({
        url: 'server/' + server + '/processlist.json',
        method: 'GET',
        error: commonError,
        success: showProcessList
      });
    }
    if (typeof cankill === 'undefined') {
      $.ajax({
        url: 'server/' + server + '/process/0',
        method: 'DELETE',
        complete: function(jqXHR) {
          cankill = (200 === jqXHR.status);
          get();
        }
      });
    } else {
      get();
    }
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
                if ($(this).text() === server) {
                  getProcessList();
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
