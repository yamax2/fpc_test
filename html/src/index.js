import './css/style.css';

var $ = require('jquery');
require('./jquery-resizable.min');

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

    var path = '/win/video/2018_01 - Тотьма/zz01-02-01-2018/';
    alert(path);
    var files = [
       '07/01021929_0001.MP4',
       '07/01021937_0002.MP4',
       '07/01021942_0003.MP4',
       '02/01021508_0012.MP4',
       '02/01021453_0009.MP4',
       '02/01021503_0011.MP4',
       '02/01021518_0014.MP4',
       '02/01021513_0013.MP4',
       '02/01021523_0015.MP4',
       '02/01021448_0008.MP4',
       '02/01021458_0010.MP4',
       '08/01021955_0005.MP4',
       '08/01022000_0006.MP4',
       '08/01021950_0004.MP4',
       '13/01022311_0020.MP4',
       '03/01021528_0016.MP4',
       '03/01021548_0020.MP4',
       '03/01021538_0018.MP4',
       '03/01021533_0017.MP4',
       '03/01021553_0021.MP4',
       '03/01021543_0019.MP4',
       '03/01021558_0022.MP4',
       '01/01021412_0002.MP4',
       '01/01021430_0006.MP4',
       '01/01021408_0001.MP4',
       '01/01021420_0004.MP4',
       '01/01021425_0005.MP4',
       '01/01021435_0007.MP4',
       '01/01021415_0003.MP4',
       '10/01022153_0012.MP4',
       '10/01022145_0011.MP4',
       '10/01022140_0010.MP4',
       '10/01022158_0013.MP4',
       '10/01022135_0009.MP4',
       '05/01021710_0035.MP4',
       '05/01021715_0036.MP4',
       '05/01021720_0037.MP4',
       '04/01021624_0026.MP4',
       '04/01021650_0031.MP4',
       '04/01021644_0030.MP4',
       '04/01021659_0033.MP4',
       '04/01021639_0029.MP4',
       '04/01021629_0027.MP4',
       '04/01021609_0023.MP4',
       '04/01021619_0025.MP4',
       '04/01021634_0028.MP4',
       '04/01021654_0032.MP4',
       '04/01021700_0034.MP4',
       '04/01021614_0024.MP4',
       '11/01022216_0016.MP4',
       '11/01022226_0018.MP4',
       '11/01022221_0017.MP4',
       '11/01022206_0014.MP4',
       '11/01022211_0015.MP4',
       '12/01022245_0019.MP4',
       '15/01030000_0027.MP4',
       '15/01022350_0025.MP4',
       '15/01022355_0026.MP4',
       '15/01022345_0024.MP4',
       '14/01022319_0021.MP4',
       '14/01022324_0022.MP4',
       '14/01022329_0023.MP4',
       '06/01021828_0038.MP4',
       '06/01021833_0039.MP4',
       '06/01021838_0040.MP4',
       '09/01022053_0007.MP4',
       '09/01022058_0008.MP4'
    ];

    files = $.map(files, function(x) {
        return path + x;
    });

    var $video = $('#video');
    $video.data('index', 0);

    var $source = $('<source type="video/mp4"></source>');
    $source
      .attr('src', files[0])
      .appendTo($video);

    $video
      .on('timeupdate', function() {
        console.log(this.currentTime);
      })
      .on('ended', function() {
        var index = $video.data('index');
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
