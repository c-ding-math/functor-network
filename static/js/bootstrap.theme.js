function dynamicModal(options){
    const modalId = 'dynamicModal_' + Date.now();

    const modalHtml = `
    <div class="modal fade" id="${modalId}" tabindex="-1" aria-hidden="true" role="dialog">
      <div class="modal-dialog" role="document">
        <div class="modal-content">
          <div class="modal-header">
            <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>
            ${options.header}
          </div>
          <div class="modal-body">
            ${options.body}
          </div>
          <div class="modal-footer">
            ${options.footer}
          </div>
        </div>
      </div>
    </div>`;

    $('body').append(modalHtml);

    var $modal = $('#' + modalId);
    $modal.modal('show');

    $modal.on('hidden.bs.modal', function () {
        $modal.remove();
    });
    return $modal;
}
