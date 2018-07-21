import './css/style.sass';

var $ = require('jquery');
require('./jquery-resizable.min');

function prepareTable() {
  var $table = $('.file-list table');
  var $row = $('thead tr:first-child', $table);
  var $tbody = $('tbody', $table);

  $.each(trips, function() {
    var $new_row = $('<tr></tr>');
    var trip = this;

    $.each($('th', $row), function() {
      $new_row.append($('<td>' + trip[$(this).data('col')] + '</td>'));
    });

    $tbody.append($new_row);
  });
}

$(document)
  .on('click', '#play', function() {
    var $video = $('#video');
    $video.data('rate', 5.0);

    var video = $video[0];

    video.playbackRate = $video.data('rate');
    video.play();
  })

  .ready(function() {
    $('.panel-right').resizable({
      handleSelector: '.splitter',
      resizeHeight: false,
      resizeWidthFrom: 'left'
    });

    $('.panel-bottom').resizable({
      handleSelector: '.splitter-horizontal',
      resizeWidth: false,
      resizeHeightFrom: 'top'
    });

    prepareTable();

    //var $source = $('<source type="video/mp4"></source>');
    //$source
    //  .attr('src', files[0])
    //  .appendTo($video);

    var $video = $('#video');
    $video.data('index', 0);

    $video
      .on('timeupdate', function() {
        console.log(this.currentTime);
      })
      .on('ended', function() {
        var index = $video.data('index');
        var files = $video.data('files') || [];

        if (index >= files.length) {
          return;
        }

        index += 1;

        $source.attr('src', files[index]);
        $video.data('index', index);

        this.load();
        this.playbackRate = $video.data('rate');
        this.play();
      });
  });
