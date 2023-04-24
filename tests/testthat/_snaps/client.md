# print-figshare

    Code
      print(cli)
    Output
      <deposits client>
       deposits service : figshare
               url_base : https://api.figshare.com/v2/
       Current deposits : 1 (see 'deposits' element for details)
      
         hostdata : <none>
         metadata : <none>

---

    Code
      cli$deposits_methods()
    Output
      List of methods for a deposits client:
      
         - deposit_delete
         - deposit_delete_file
         - deposit_download_file
         - deposit_embargo
         - deposit_fill_metadata
         - deposit_new
         - deposit_publish
         - deposit_retrieve
         - deposit_service
         - deposit_service_parameters
         - deposit_update
         - deposit_update_frictionless
         - deposit_upload_file
         - deposits_list
         - deposits_methods
         - deposits_search
       
       see `?depositsClient` for full details of all methods.

---

    Code
      print(cli)
    Output
      <deposits client>
       deposits service : figshare
               url_base : https://api.figshare.com/v2/
       Current deposits : 5 (see 'deposits' element for details)
      
      url_service : https://my.deposit
       deposit id : 1
         hostdata : list with 1  elements
         metadata : 1 terms (see 'metadata' element for details)

# print-zenodo

    Code
      print(cli)
    Output
      <deposits client>
       deposits service : zenodo
                 sandbox: TRUE
               url_base : https://sandbox.zenodo.org/api/
       Current deposits : 2 (see 'deposits' element for details)
      
         hostdata : <none>
         metadata : <none>

---

    Code
      cli$deposits_methods()
    Output
      List of methods for a deposits client:
      
         - deposit_delete
         - deposit_delete_file
         - deposit_download_file
         - deposit_embargo
         - deposit_fill_metadata
         - deposit_new
         - deposit_publish
         - deposit_retrieve
         - deposit_service
         - deposit_service_parameters
         - deposit_update
         - deposit_update_frictionless
         - deposit_upload_file
         - deposits_list
         - deposits_methods
         - deposits_search
       
       see `?depositsClient` for full details of all methods.

---

    Code
      print(cli)
    Output
      <deposits client>
       deposits service : zenodo
                 sandbox: TRUE
               url_base : https://sandbox.zenodo.org/api/
       Current deposits : 5 (see 'deposits' element for details)
      
      url_service : https://my.deposit
       deposit id : 1
         hostdata : list with 1  elements
         metadata : 1 terms (see 'metadata' element for details)

