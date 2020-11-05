INTERFACE if_abapgit_repository_auth
  PUBLIC .

  "! Checks if the repository is PRIVATE or PUBLIC
  METHODS determine_access_level
    IMPORTING !iv_url               TYPE string
    RETURNING VALUE(rv_repo_access) TYPE string
    RAISING   cx_abapgit_exception .

  "! Handle exception arising due to authentication issue
  METHODS handle_auth_exception
    IMPORTING ix_abapgit_exception TYPE REF TO cx_abapgit_exception
              iv_http_status       TYPE        i
    RAISING   cx_adt_rest_abapgit .

  "! <p>Checks whether the abapgit exception which is raised is because of any authentication related
  "! issues on the git server</p>
  METHODS is_authorization_issue
    IMPORTING ix_abapgit_exception    TYPE REF TO cx_abapgit_exception
    EXPORTING ev_http_status          TYPE i
    RETURNING VALUE(rv_is_auth_issue) TYPE abap_bool.

ENDINTERFACE.
