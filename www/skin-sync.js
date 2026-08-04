// www/skin-sync.js
(function() {
  "use strict";

  // Returns the background color of the active sidebar nav item (non-transparent)
  function getActiveNavItemColor() {
    var el = document.querySelector('.nav-sidebar .nav-link.active');
    if (!el) return null;
    var color = window.getComputedStyle(el).backgroundColor;
    if (!color || color === 'transparent' || color === 'rgba(0, 0, 0, 0)') return null;
    return color;
  }

  // compute luminance for contrast
  function rgbToLuminance(rgb) {
    var nums = (rgb.match(/\d+/g) || []).map(Number).slice(0, 3);
    if (nums.length < 3) return 1;
    var r = nums[0] / 255, g = nums[1] / 255, b = nums[2] / 255;
    [r,g,b] = [r,g,b].map(function(v) {
      return v <= 0.03928 ? v / 12.92 : Math.pow((v + 0.055) / 1.055, 2.4);
    });
    return 0.2126 * r + 0.7152 * g + 0.0722 * b;
  }

  // Apply color to the card (.card, .card-header, .card-body)
  function applyColorToBox(wrapperId, color) {
    if (!color) return;
    var wrapper = document.getElementById(wrapperId);
    if (!wrapper) return;
    var card = wrapper.querySelector('.card');
    if (!card) return;

    var header = card.querySelector('.card-header');
    var body = card.querySelector('.card-body');

    card.style.backgroundColor = color;
    if (header) header.style.backgroundColor = color;
    if (body) body.style.backgroundColor = color;

    var lum = rgbToLuminance(color);
    var textColor = lum > 0.5 ? 'rgba(0,0,0,0.85)' : '#ffffff';

    card.style.color = textColor;
    if (header) header.style.color = textColor;
    if (body) body.style.color = textColor;

    card.style.transition = 'background-color 200ms ease, color 200ms ease';
    if (header) header.style.transition = 'background-color 200ms ease, color 200ms ease';
    if (body) body.style.transition = 'background-color 200ms ease, color 200ms ease';
  }

  var themedBoxIds = ['themed-box-1', 'themed-box-2', 'themed-box-3', 'themed-box-4', 'themed-box-5', 'themed-box-6'];

  function applyColorToAllBoxes() {
    var color = getActiveNavItemColor();
    if (!color) return;
    themedBoxIds.forEach(function(id) {
      applyColorToBox(id, color);
    });
  }

  // Debounced wrapper
  function scheduleApply(delay) {
    clearTimeout(window.__applyBoxesTimeout);
    window.__applyBoxesTimeout = setTimeout(applyColorToAllBoxes, delay || 30);
  }

  // Run on DOMContentLoaded and a couple of retries
  document.addEventListener('DOMContentLoaded', function() {
    scheduleApply(20);
    setTimeout(function() { scheduleApply(120); }, 120);
    setTimeout(function() { scheduleApply(600); }, 600);
  });

  // Observe body.class changes (fallback)
  var bodyObserver = new MutationObserver(function(mutations) {
    var changed = mutations.some(function(m) {
      return m.type === 'attributes' && m.attributeName === 'class';
    });
    if (changed) scheduleApply(20);
  });
  bodyObserver.observe(document.body, { attributes: true });

  // Observe the sidebar subtree for any changes (class/style/childList) -> catches updateSidebarTheme actions
  function observeSidebarIfExists() {
    var sidebar = document.querySelector('.main-sidebar, .sidebar, .nav-sidebar');
    if (!sidebar) return;
    var obs = new MutationObserver(function(mutations) {
      // any change in subtree likely indicates the active nav item or its style changed
      scheduleApply(20);
    });
    obs.observe(sidebar, { attributes: true, childList: true, subtree: true });
    // keep a reference so GC doesn't remove it (and avoid repeated observers)
    window.__sidebarObserver = obs;
  }
  // try to attach observer now and shortly after in case sidebar is created async
  observeSidebarIfExists();
  setTimeout(observeSidebarIfExists, 120);
  setTimeout(observeSidebarIfExists, 600);

  // Also listen for clicks on the theme chips; apply right after the click
  document.addEventListener('click', function(e) {
    var chip = e.target.closest('.sidebar-themer-chip, .accents-themer-chip');
    if (chip) {
      // update after slight delay so theme update code runs first
      scheduleApply(30);
    }
  }, true);

  // Expose manual trigger for debugging
  window.applySkinToAllThemedBoxes = applyColorToAllBoxes;

})();

