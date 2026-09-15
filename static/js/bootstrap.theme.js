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

function applyAppearance () {
  const savedappearance = localStorage.getItem('appearance');
  const systemPrefersDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
  const initialappearance = savedappearance || (systemPrefersDark ? 'dark' : 'light');
  document.documentElement.setAttribute('data-appearance', initialappearance);
}

applyAppearance();

window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', e => {
  applyAppearance();
});

document.addEventListener('DOMContentLoaded', () => {
  const dropdownItems = document.querySelectorAll('[data-appearance-value]');
  const savedSetting = localStorage.getItem('appearance');

  function updateDropdownUI(currentSetting) {
    const currentSetting0 = currentSetting || 'system'
    dropdownItems.forEach(item => {
      let itemParent = item.parentElement;
      itemParent.classList.remove('active');
      if (item.getAttribute('data-appearance-value') === currentSetting0) {
        itemParent.classList.add('active'); 
      } 
    });
  }
  updateDropdownUI(savedSetting);

  dropdownItems.forEach(item => {
    item.addEventListener('click', (e) => {
      e.preventDefault(); 
      
      const newSetting = item.getAttribute('data-appearance-value');
      if (newSetting == 'system') {
        localStorage.removeItem('appearance');
      } else {
        localStorage.setItem('appearance', newSetting);
      }
      updateDropdownUI(newSetting); 
      applyAppearance();
    });
  });  
});

document.addEventListener('DOMContentLoaded', () => {
  const radioItems = document.querySelectorAll('[name="appearance"]');
  const savedSetting = localStorage.getItem('appearance');

  function updateRadioUI(currentSetting) {
    const currentSetting0 = currentSetting || 'system'
    radioItems.forEach(item => {
      if (item.value === currentSetting0) {
        item.checked = true;
      } 
    });
  }
  updateRadioUI(savedSetting);

  radioItems.forEach(item => {
    item.addEventListener('click', (e) => {
      //e.preventDefault(); 
      const newSetting = item.value;
      if (newSetting == 'system') {
        localStorage.removeItem('appearance');
      } else {
        localStorage.setItem('appearance', newSetting);
      }
      updateRadioUI(newSetting); 
      applyAppearance();
    });
  });  
});