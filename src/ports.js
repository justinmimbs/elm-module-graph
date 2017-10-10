function requestFileInputData(id, callback) {
  var input = document.getElementById(id);
  if (input === null || input.type !== 'file') {
    return;
  }

  var maxFileSize = 512 * 1024;
  var file = input.files[0];

  if (file === undefined) {
    callback([id, null]);

  } else if (file.size > maxFileSize) {
    callback([id, {
      name : file.name,
      error: 'File is larger than the maximum allowed (' + Math.round(maxFileSize / 1024) + ' KB)',
      content: null
    }]);

  } else {
    var fileReader = new FileReader();

    fileReader.addEventListener('error', function () {
      callback([id, {
        name: file.name,
        error: 'File could not be loaded: ' + fileReader.error.name,
        content: null
      }]);
    });

    fileReader.addEventListener('load', function () {
      callback([id, {
        name: file.name,
        error: null,
        content: fileReader.result
      }]);
    });

    fileReader.readAsText(file);
  }
}

module.exports = {
  init: function (app) {
    app.ports.requestFileInputData.subscribe(function (id) {
      requestFileInputData(id, app.ports.fileInputData.send);
    });
  }
};
