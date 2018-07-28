import './css/style.sass';

var $ = require('jquery');
require('./jquery-resizable.min');

var selected = 0;

function prepareTable() {
  var $table = $('.file-list table');
  var $row = $('thead tr:first-child', $table);
  var $tbody = $('tbody', $table);

  $.each(trips, function(i) {
    var $new_row = $('<tr data-index="' + i + '"></tr>');
    var trip = this;

    $.each($('th', $row), function() {
      $new_row.append($('<td>' + trip[$(this).data('col')] + '</td>'));
    });

    $tbody.append($new_row);
  });
}

function selectRow(index) {
  var $table = $('.file-list table');
  var $selected = $('.selected', $table);

  if (($selected.length > 0) && ($selected.data('index') === index)) {
    return;
  }

  $selected.removeClass('selected');
  $('tr[data-index="' + index + '"]', $table).addClass('selected');
}

$(document)
  .on('click', '.file-list table tr[data-index] td', function() {
    selected = $(this).closest('tr').data('index');
    selectRow(selected);
   })
  .on('click', '#play', function() {
    var $video = $('#video');
    $video.data('rate', 5.0);

    var video = $video[selected];

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
    selectRow(selected);

    //var $source = $('<source type="video/mp4"></source>');
    //$source
    //  .attr('src', files[0])
    //  .appendTo($video);

    var $video = $('#video');
    $video.data('index', selected);

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
