REPORT z_algj_30.

*$*$ -------------------------------------------------------------- *$*$
*$*$                         Declarações                            *$*$
*$*$ -------------------------------------------------------------- *$*$

TABLES: vbak.

TYPES:

  BEGIN OF ty_vbak,
    vbeln TYPE vbak-vbeln,
    erdat TYPE vbak-erdat,
    netwr TYPE vbak-netwr,
    kunnr TYPE vbak-kunnr,
  END OF ty_vbak,

  BEGIN OF ty_vbap,
    vbeln TYPE vbap-vbeln,
    posnr TYPE vbap-posnr,
    matnr TYPE vbap-matnr,
    gsber TYPE vbap-gsber,
  END OF ty_vbap,

  BEGIN OF ty_lips,
    vbeln TYPE lips-vbeln,
    posnr TYPE lips-posnr,
    vgbel TYPE lips-vgbel,
    vgpos TYPE lips-vgpos,
  END OF ty_lips,

  BEGIN OF ty_kna1,
    kunnr TYPE kna1-kunnr,
    name1 TYPE kna1-name1,
  END OF ty_kna1,

  BEGIN OF ty_makt,
    matnr TYPE makt-matnr,
    spras TYPE makt-spras,
    maktx TYPE makt-maktx,
  END OF ty_makt,

  BEGIN OF ty_tgsbt,
    spras TYPE tgsbt-spras,
    gsber TYPE tgsbt-gsber,
    gtext TYPE tgsbt-gtext,
  END OF ty_tgsbt,

  BEGIN OF ty_saida,
    vbeln_vbak  TYPE vbak-vbeln,
    erdat       TYPE vbak-erdat,
    posnr       TYPE vbap-posnr,
    kunnr_name1 TYPE char64,
    netwr       TYPE vbak-netwr,
    matnr_maktx TYPE char64,
    gsber_gtext TYPE char64,
    vbeln_lips  TYPE lips-vbeln,
    status      TYPE icon-id,
    flag        TYPE flag,
  END OF ty_saida.

DATA: ti_vbak       TYPE TABLE OF ty_vbak,
      ti_vbap       TYPE TABLE OF ty_vbap,
      ti_lips       TYPE TABLE OF ty_lips,
      ti_kna1       TYPE TABLE OF ty_kna1,
      ti_makt       TYPE TABLE OF ty_makt,
      ti_tgsbt      TYPE TABLE OF ty_tgsbt,
      ti_saida      TYPE TABLE OF ty_saida,
      ti_sort       TYPE TABLE OF slis_sortinfo_alv,
      ti_listheader TYPE TABLE OF slis_listheader,
      ti_fieldcat   TYPE TABLE OF slis_fieldcat_alv.

DATA: wa_vbak       TYPE ty_vbak,
      wa_vbap       TYPE ty_vbap,
      wa_lips       TYPE ty_lips,
      wa_kna1       TYPE ty_kna1,
      wa_makt       TYPE ty_makt,
      wa_tgsbt      TYPE ty_tgsbt,
      wa_saida      TYPE ty_saida,
      wa_sort       TYPE slis_sortinfo_alv,
      wa_fieldcat   TYPE slis_fieldcat_alv,
      wa_listheader TYPE slis_listheader.


CONSTANTS: c_ta                  TYPE char2         VALUE 'TA',
           c_eur                 TYPE char3         VALUE 'EUR',
           c_separador1          TYPE char1         VALUE ';',
           c_separador2          TYPE char1         VALUE '/',
           c_separador3          TYPE char1         VALUE ':',
           c_separador4          TYPE char1         VALUE '|',
           c_tan                 TYPE char3         VALUE 'TAN',
           c_e                   TYPE char3         VALUE 'E',
           c_s                   TYPE char3         VALUE 'S',
           c_x                   TYPE char3         VALUE 'X',
           c_caminho_csv         TYPE string        VALUE 'D:\Download\teste.CSV',
           c_caminho_txt         TYPE string        VALUE 'C:\Users\Andr� LGJ\Desktop\Relat�rio documentos de vendas.TXT',
           c_asc                 TYPE char10        VALUE 'ASC',
           c_standard_fullscreen TYPE char19        VALUE 'STANDARD_FULLSCREEN',
           c_exp_txt             TYPE char7         VALUE 'EXP_TXT',
           c_exp_csv             TYPE char7         VALUE 'EXP_CSV',
           c_vbeln_vbak          TYPE char10        VALUE 'VBELN_VBAK',
           c_aun                 TYPE char3         VALUE 'AUN',
           c_va03                TYPE char4         VALUE 'VA03',
           c_vbeln_lips          TYPE char10        VALUE 'VBELN_LIPS',
           c_vl                  TYPE char2         VALUE 'VL',
           c_vl03n               TYPE char5         VALUE 'VL03N',
           c_ti_saida            TYPE char8         VALUE 'TI_SAIDA',
           c_erdat               TYPE char5         VALUE 'ERDAT',
           c_posnr               TYPE char5         VALUE 'POSNR',
           c_kunnr_name1         TYPE char11        VALUE 'KUNNR_NAME1',
           c_netwr               TYPE char5         VALUE 'NETWR',
           c_matnr_maktx         TYPE char11        VALUE 'MATNR_MAKTX',
           c_gsber_gtext         TYPE char11        VALUE 'GSBER_GTEXT',
           c_status              TYPE char6         VALUE 'STATUS',
           c_vbeln               TYPE char5         VALUE 'VBELN',
           c_id                  TYPE char2         VALUE 'ID',
           c_vbak                TYPE char4         VALUE 'VBAK',
           c_vbap                TYPE char4         VALUE 'VBAP',
           c_lips                TYPE char4         VALUE 'LIPS',
           c_icon                TYPE char4         VALUE 'ICON',
           c_cliente             TYPE char7         VALUE 'Cliente',
           c_material            TYPE char8         VALUE 'Material',
           c_divisao             TYPE char7         VALUE 'Divisão',
           c_flag                TYPE char4         VALUE 'FLAG',
           c_zf_status           TYPE slis_formname VALUE 'ZF_STATUS',
           c_zf_user_command     TYPE slis_formname VALUE 'ZF_USER_COMMAND',
           c_zf_top_of_page      TYPE slis_formname VALUE 'ZF_TOP_OF_PAGE',
           c_vermelho            TYPE char4         VALUE '@0A@',
           c_amarelo             TYPE char4         VALUE '@09@',
           c_verde               TYPE char4         VALUE '@08@'.

*$*$ -------------------------------------------------------------- *$*$
*$*$                            TELA                                *$*$
*$*$ -------------------------------------------------------------- *$*$

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001. "Tela de seleção

SELECT-OPTIONS:
  s_vbeln FOR vbak-vbeln,
  s_erdat FOR vbak-erdat OBLIGATORY DEFAULT '20080101' TO '20081231', "Para definir a data como default precisa colocar no padrão do sap
  s_kunnr FOR vbak-kunnr.

SELECTION-SCREEN: END OF BLOCK b1.

*$*$ -------------------------------------------------------------- *$*$
*$*$                           EVENTOS                              *$*$
*$*$ -------------------------------------------------------------- *$*$

START-OF-SELECTION.
  PERFORM zf_seleciona_dados.

END-OF-SELECTION.
  PERFORM: zf_processa_dados,
           zf_monta_alv,
           zf_quebra_de_campo,
           zf_mostra_alv.

*$*$ -------------------------------------------------------------- *$*$
*$*$                            FORMS                               *$*$
*$*$ -------------------------------------------------------------- *$*$

FORM zf_seleciona_dados.

  FREE ti_vbak.
  SELECT vbeln
         erdat
         netwr
         kunnr
    FROM vbak
    INTO TABLE ti_vbak
   WHERE vbeln IN s_vbeln
     AND erdat IN s_erdat
     AND kunnr IN s_kunnr
     AND auart =  c_ta
  AND waerk =  c_eur.

  IF sy-subrc <> 0.
    FREE ti_vbak.
    MESSAGE text-002 TYPE c_s DISPLAY LIKE c_e.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF ti_vbak IS NOT INITIAL.

    FREE ti_vbap.
    SELECT vbeln
           posnr
           matnr
           gsber
      FROM vbap
      INTO TABLE ti_vbap
       FOR ALL ENTRIES IN  ti_vbak
    WHERE vbeln = ti_vbak-vbeln.

    IF sy-subrc <> 0.
      FREE ti_vbap.
    ENDIF.

    DATA(ti_vbak_aux) = ti_vbak.
    SORT ti_vbak_aux BY kunnr.
    DELETE ADJACENT DUPLICATES FROM ti_vbak_aux COMPARING kunnr.

    FREE ti_kna1.
    SELECT kunnr  " N� cliente
           name1  " Nome 1
      FROM kna1
      INTO TABLE ti_kna1
       FOR ALL ENTRIES IN ti_vbak_aux
    WHERE kunnr = ti_vbak_aux-kunnr.

    IF sy-subrc <> 0.
      FREE ti_kna1.
    ENDIF.



  ENDIF. "ti_vbak IS NOT INITIAL

  IF ti_vbap IS NOT INITIAL.

    FREE ti_lips.
    SELECT vbeln
           posnr
           vgbel
           vgpos
      FROM lips
      INTO TABLE ti_lips
       FOR ALL ENTRIES IN ti_vbap
     WHERE vgbel = ti_vbap-vbeln
       AND vgpos = ti_vbap-posnr
    AND pstyv = c_tan.

    IF sy-subrc <> 0.
      FREE ti_lips.
    ENDIF.

    FREE ti_makt.
    SELECT matnr
           spras
           maktx
      FROM makt
      INTO TABLE ti_makt
       FOR ALL ENTRIES IN ti_vbap
     WHERE matnr = ti_vbap-matnr
    AND spras = sy-langu.

    IF sy-subrc <> 0.
      FREE ti_makt.
    ENDIF.

    FREE ti_tgsbt.
    SELECT spras
           gsber
           gtext
      FROM tgsbt
      INTO TABLE ti_tgsbt
       FOR ALL ENTRIES IN ti_vbap
     WHERE spras = sy-langu
    AND gsber = ti_vbap-gsber.

    IF sy-subrc <> 0.
      FREE ti_tgsbt.
    ENDIF.

  ENDIF. "ti_vbap IS NOT INITIAL

ENDFORM. "zf_seleciona_dados

FORM zf_processa_dados.

  SORT: ti_vbap  BY vbeln,
        ti_vbak  BY vbeln,
        ti_kna1  BY kunnr,
        ti_lips  BY vgbel
                    vgpos,
        ti_makt  BY matnr,
        ti_tgsbt BY gsber.

  LOOP AT ti_vbap INTO wa_vbap.

    READ TABLE ti_vbak INTO wa_vbak WITH KEY
                                            vbeln = wa_vbap-vbeln BINARY SEARCH.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    READ TABLE ti_kna1 INTO wa_kna1 WITH KEY
                                              kunnr = wa_vbak-kunnr BINARY SEARCH.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    READ TABLE ti_lips INTO wa_lips WITH KEY
                                            vgbel = wa_vbap-vbeln
                                            vgpos = wa_vbap-posnr BINARY SEARCH.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    READ TABLE ti_makt INTO wa_makt WITH KEY
                                            matnr = wa_vbap-matnr BINARY SEARCH.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    READ TABLE ti_tgsbt INTO wa_tgsbt WITH KEY
                                              gsber = wa_vbap-gsber BINARY SEARCH.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    wa_saida-vbeln_vbak  = wa_vbak-vbeln.
    wa_saida-erdat       = wa_vbak-erdat.
    wa_saida-posnr       = wa_vbap-posnr.

    CONCATENATE wa_vbak-kunnr
                wa_kna1-name1
    INTO wa_saida-kunnr_name1
    SEPARATED BY ';'.

    wa_saida-netwr       = wa_vbak-netwr.

    CONCATENATE wa_vbap-matnr
                wa_makt-maktx
    INTO wa_saida-matnr_maktx
    SEPARATED BY ';'.

    CONCATENATE wa_vbap-gsber
                wa_tgsbt-gtext
    INTO wa_saida-gsber_gtext
    SEPARATED BY ';'.

    wa_saida-vbeln_lips  = wa_lips-vbeln.

    IF wa_vbak-netwr <= 20000.
      wa_saida-status      = c_vermelho. "Vermelho
    ELSEIF wa_vbak-netwr > 20000 AND wa_vbak-netwr <= 40000.
      wa_saida-status      = c_amarelo. "Amarelo
    ELSE.
      wa_saida-status      = c_verde. "Verde
    ENDIF.

    APPEND wa_saida TO ti_saida.
    CLEAR wa_saida.

  ENDLOOP. " LOOP AT ti_vbap INTO wa_vbap

ENDFORM.

FORM zf_monta_alv.

  PERFORM zf_monta_fieldcat USING:
  c_vbeln_vbak     c_ti_saida      c_vbeln  c_vbak  c_x c_x '',
  c_erdat          c_ti_saida      c_erdat  c_vbak  ''  ''  '',
  c_posnr          c_ti_saida      c_posnr  c_vbap  ''  ''  '',
  c_kunnr_name1    c_ti_saida      ''       ''      ''  ''  c_cliente,
  c_netwr          c_ti_saida      c_netwr  c_vbak  ''  c_x '',
  c_matnr_maktx    c_ti_saida      ''       ''      ''  ''  c_material,
  c_gsber_gtext    c_ti_saida      ''       ''      ''  ''  c_divisao,
  c_vbeln_lips     c_ti_saida      c_vbeln  c_lips  c_x ''  '',
  c_status         c_ti_saida      c_id     c_icon  ''  ''  c_status.

ENDFORM. "zf_monta_alv

FORM zf_monta_fieldcat USING field        TYPE any
                             tab          TYPE any
                             ref_field    TYPE any
                             ref_tab      TYPE any
                             hotspot      TYPE any
                             sum          TYPE any
                             reptext_ddic TYPE any.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname     = field.
  wa_fieldcat-tabname       = tab.
  wa_fieldcat-ref_fieldname = ref_field.
  wa_fieldcat-ref_tabname   = ref_tab.
  wa_fieldcat-hotspot       = hotspot.
  wa_fieldcat-do_sum        = sum.
  wa_fieldcat-reptext_ddic  = reptext_ddic.

  APPEND wa_fieldcat TO ti_fieldcat.

ENDFORM. "zf_monta_fieldcat

FORM zf_mostra_alv.

  DATA:           wa_layout TYPE slis_layout_alv.

  wa_layout-expand_all = abap_true.
  wa_layout-colwidth_optimize = abap_true.
  wa_layout-box_fieldname = c_flag.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = c_zf_status
      i_callback_user_command  = c_zf_user_command
      i_callback_top_of_page   = c_zf_top_of_page
      is_layout                = wa_layout
      it_fieldcat              = ti_fieldcat
      it_sort                  = ti_sort
    TABLES
      t_outtab                 = ti_saida
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE text-003 TYPE c_s DISPLAY LIKE c_e.
    LEAVE LIST-PROCESSING.
  ENDIF.


ENDFORM. "zf_mostra_alv
*
FORM zf_quebra_de_campo.

  FREE ti_sort.

  CLEAR wa_sort.
  wa_sort-spos      = 1.
  wa_sort-fieldname = c_erdat.
  wa_sort-tabname   = c_ti_saida.
  wa_sort-up        = abap_true.
  wa_sort-subtot    = abap_true.
  APPEND wa_sort TO ti_sort.

  CLEAR wa_sort.
  wa_sort-spos      = 2.
  wa_sort-fieldname = c_kunnr_name1.
  wa_sort-tabname   = c_ti_saida.
  wa_sort-up        = abap_true.
  wa_sort-subtot    = abap_true.
  APPEND wa_sort TO ti_sort.

  CLEAR wa_sort.
  wa_sort-spos      = 3.
  wa_sort-fieldname = c_gsber_gtext.
  wa_sort-tabname   = c_ti_saida.
  wa_sort-up        = abap_true.
  wa_sort-subtot    = abap_true.
  APPEND wa_sort TO ti_sort.

ENDFORM. "form zf_quebra_de_campo

##CALLED
FORM zf_top_of_page.

  DATA: data      TYPE char10,
        hora      TYPE char5,
        timestamp TYPE char30.

  FREE ti_listheader.

  CONCATENATE sy-datum+6(2)
              sy-datum+4(2)
              sy-datum+0(4)
             INTO  data SEPARATED BY c_separador2.

  CONCATENATE sy-uzeit+0(2)
              sy-uzeit+2(2)
             INTO hora SEPARATED BY c_separador3.

  CONCATENATE sy-repid c_separador4 data hora INTO timestamp SEPARATED BY space.

  CLEAR wa_listheader.
  wa_listheader-typ  = c_s.
  wa_listheader-info = timestamp.
  APPEND wa_listheader TO ti_listheader.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = ti_listheader.


ENDFORM. "zf_top_of_page

##CALLED
FORM zf_status USING pf_tab TYPE slis_t_extab.
  SET PF-STATUS c_standard_fullscreen.
ENDFORM. "zf_status

##CALLED
FORM zf_user_command USING vl_ucomm    LIKE sy-ucomm
                          rs_selfield TYPE slis_selfield.


  CASE vl_ucomm.
    WHEN c_exp_txt.

      PERFORM zf_exporta_txt.

    WHEN c_exp_csv.

      PERFORM zf_exporta_csv.

    WHEN OTHERS.

      IF rs_selfield-fieldname = c_vbeln_vbak.
        READ TABLE ti_saida INTO wa_saida INDEX rs_selfield-tabindex.
        IF sy-subrc IS INITIAL.
          SET PARAMETER ID c_aun FIELD rs_selfield-value.
          CALL TRANSACTION c_va03 AND SKIP FIRST SCREEN.
        ENDIF.
      ELSE.
        rs_selfield-fieldname = c_vbeln_lips.
        READ TABLE ti_saida INTO wa_saida INDEX rs_selfield-tabindex.
        IF sy-subrc IS INITIAL.
          SET PARAMETER ID c_vl FIELD rs_selfield-value.
          CALL TRANSACTION c_vl03n AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.

  ENDCASE.

ENDFORM. "z_user_command

FORM zf_exporta_txt.

  DATA: ti_saida_aux TYPE TABLE OF ty_saida.

  ti_saida_aux = ti_saida.
  DELETE ti_saida_aux WHERE flag <> abap_true.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = c_caminho_txt
    TABLES
      data_tab                = ti_saida_aux
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.
  IF sy-subrc <> 0.
    MESSAGE text-011 TYPE c_s DISPLAY LIKE c_e. "'Erro ao salvar o arquivo!
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM. "zf_exporta_txt


FORM zf_exporta_csv.

  DATA: ti_saida_csv TYPE TABLE OF string,
        wa_saida_csv TYPE string,
        lv_dmbtr     TYPE string,
        lv_netwr     TYPE string.

  CLEAR wa_saida_csv.

  CONCATENATE text-003    " Documento de vendas
              text-004    " Data de cria��o do registro
              text-005    " Item do documento de vendas
              text-006    " N� cliente e nome
              text-007    " Valor l�quido da ordem na moeda do documento
              text-008    " Material
              text-009    " Divis�o
              text-010    " Fornecimento

  INTO wa_saida_csv
  SEPARATED BY c_separador1.

  APPEND wa_saida_csv TO ti_saida_csv.

  LOOP AT ti_saida INTO wa_saida.

    IF wa_saida-flag <> abap_true.
      CONTINUE.
    ENDIF.

    CLEAR wa_saida_csv.
    CONCATENATE wa_saida-vbeln_vbak
                wa_saida-erdat
                wa_saida-posnr
                wa_saida-kunnr_name1
                lv_netwr
                wa_saida-matnr_maktx
                wa_saida-gsber_gtext
                wa_saida-vbeln_lips

    INTO wa_saida_csv
    SEPARATED BY ';'.

    APPEND wa_saida_csv TO ti_saida_csv.

  ENDLOOP.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = c_caminho_csv
      filetype                = c_asc
    TABLES
      data_tab                = ti_saida_csv
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.

  IF sy-subrc <> 0.
    MESSAGE text-004 TYPE c_s DISPLAY LIKE c_e. "'Erro ao salvar o arquivo!
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM. "zf_exporta_csv