function updateFooter(tfoot) {
  const td = tfoot.querySelector("td");
  if (td) {
    tfoot.innerHTML = `<td colspan="100%" style="text-align: left;">${td.innerHTML}</td>`;
  }
  tfoot.style.fontSize = "0.9em";
}

function adjustFooter() {
  document.querySelectorAll("tfoot").forEach(updateFooter);
}

function styleTables(selector, maxWidth) {
  document.querySelectorAll(selector).forEach(element => {
    element.style.maxWidth = maxWidth;
    element.style.margin = '0 auto';
  });
}

function highlightCodeSyntax() {
  const keywords = [
    { text: '(', newClass: 'bracket' },
    { text: ')', newClass: 'bracket' },
    { text: '%>%', newClass: 'pipe' },
    { text: '$', newClass: 'dollarsign' },
    { text: 'function', newClass: 'function' },
    { text: 'source', newClass: 'source' },
    { text: ',', newClass: 'comma' },
    { text: '::', newClass: 'doublecolon' },
    { text: 'else', newClass: 'else' },
    { text: 'if', newClass: 'if' },
    { text: '~', newClass: 'tilde' },
    { text: '=', newClass: 'equals' },
    { text: '%in%', newClass: 'in' },
    { text: '%$%', newClass: 'exposition-pipe' },
    { text: '&', newClass: 'ampersand' },
    { text: '-', newClass: 'minus' },
    { text: '+', newClass: 'plus' },
    { text: '/', newClass: 'slash' },
    { text: '*', newClass: 'asterisk' }
  ];

  document.querySelectorAll('pre').forEach(pre => {
    pre.style.setProperty('line-height', 'normal', 'important');
    const codeElement = pre.querySelector('code');
    if (codeElement) {
      codeElement.querySelectorAll('span').forEach(line => {
        const keyword = keywords.find(k => k.text === line.textContent);
        if (keyword) {
          line.classList.add(keyword.newClass);
        }
      });
    }
  });
}

function highlightPackages() {
  document.querySelectorAll('pre code').forEach(codeBlock => {
    const spans = Array.from(codeBlock.querySelectorAll('span.fu'));
    spans.forEach((span, index) => {
      const nextSpan = spans[index + 1];
      if (nextSpan && nextSpan.textContent === '::') {
        span.classList.add('package');
      }
    });
  });
}

function wrapCommasInSpans() {
  document.querySelectorAll('pre code').forEach(codeBlock => {
    const fragment = document.createDocumentFragment();
    const tempDiv = document.createElement('div');
    tempDiv.innerHTML = codeBlock.innerHTML;

    function processNode(node) {
      if (node.nodeType === Node.TEXT_NODE) {
        const newContent = document.createDocumentFragment();
        node.textContent.split(',').forEach((part, index, array) => {
          newContent.appendChild(document.createTextNode(part));
          if (index < array.length - 1) {
            const commaSpan = document.createElement('span');
            commaSpan.textContent = ',';
            commaSpan.className = 'comma';
            newContent.appendChild(commaSpan);
          }
        });
        return newContent;
      } else if (node.nodeType === Node.ELEMENT_NODE) {
        const clone = node.cloneNode(false);
        if (node.className.includes('st')) {
          clone.textContent = node.textContent;
        } else {
          for (let child of node.childNodes) {
            clone.appendChild(processNode(child));
          }
        }
        return clone;
      }
      return node.cloneNode(true);
    }

    for (let child of tempDiv.childNodes) {
      fragment.appendChild(processNode(child));
    }
    codeBlock.innerHTML = '';
    codeBlock.appendChild(fragment);
  });
}

function initializeFeatures() {
  document.querySelectorAll("tfoot").forEach(tfoot => {
    updateFooter(tfoot);
    new MutationObserver(mutations => {
      if (mutations.some(m => m.type === "attributes" && m.attributeName === "colspan")) {
        updateFooter(tfoot);
      }
    }).observe(tfoot, { attributes: true, subtree: true, attributeFilter: ["colspan"] });
  });

  document.addEventListener('click', event => {
    if (event.target.matches('[class*=paginate_button]')) {
      adjustFooter();
    }
  });

  styleTables('.table-max-450 table', '450px');
  styleTables('.table-max-550 table', '550px');
  highlightCodeSyntax();
  highlightPackages();
  wrapCommasInSpans();
}

document.addEventListener('DOMContentLoaded', () => {
  initializeFeatures();

});


// new MutationObserver(() => {
//   initializeFeatures();
// }).observe(document.body, { childList: true, subtree: true });