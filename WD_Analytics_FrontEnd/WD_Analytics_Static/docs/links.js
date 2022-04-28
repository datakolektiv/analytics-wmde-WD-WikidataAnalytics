(function() {
  for (const link of document.getElementsByTagName('a')) {
    if (/^(http?:)?\/\//.test(link.getAttribute('href'))) link.target = '_blank';
  }
})();