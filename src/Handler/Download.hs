{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Download where

import Import

downloadWidget :: EntryId -> Widget
downloadWidget entryId = toWidget
    [julius|
let route = "@{DownloadR entryId}";
function downloadFile() {
    fetch('route')
        .then(response => response.blob())
        .then(blob => {
            const url = window.URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url;
            a.download = "example.pdf";  // Change filename if necessary
            document.body.appendChild(a);
            a.click();
            a.remove();
        })
        .catch(error => console.error('Download error:', error));
}
$('download a').click(downloadFile);
    |]