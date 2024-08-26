REPORT z_algj_29.

*$*$ -------------------------------------------------------------- *$*$
*$*$                         Declarações                            *$*$
*$*$ -------------------------------------------------------------- *$*$

TABLES: mkpf,
        mseg.

TYPES:

  BEGIN OF ty_mkpf,
    mblnr TYPE mkpf-mblnr,
    mjahr TYPE mkpf-mjahr,
    bldat TYPE mkpf-bldat,
  END OF ty_mkpf,

  BEGIN OF ty_mseg,
    mblnr TYPE mseg-mblnr,
    mjahr TYPE mseg-mjahr,
    zeile TYPE mseg-zeile,
    bwart TYPE mseg-bwart,
    matnr TYPE mseg-matnr,
    werks TYPE mseg-werks,
    lgort TYPE mseg-lgort,
    dmbtr TYPE mseg-dmbtr,
    menge TYPE mseg-menge,
    meins	TYPE mseg-meins,
  END OF ty_mseg,

  BEGIN OF ty_makt,
    matnr TYPE makt-matnr,
    spras TYPE makt-spras,
    maktx TYPE makt-maktx,
  END OF ty_makt,

  BEGIN OF ty_t001w,
    werks TYPE t001w-werks,
    name1 TYPE t001w-name1,
  END OF ty_t001w,

  BEGIN OF ty_t001l,
    werks TYPE t001l-werks,
    lgort TYPE t001l-lgort,
    lgobe TYPE t001l-lgobe,
  END OF ty_t001l,

  BEGIN OF ty_saida,
    mblnr          TYPE mkpf-mblnr,
    mjahr          TYPE mkpf-mjahr,
    zeile          TYPE mseg-zeile,
    bwart          TYPE mseg-bwart,
    bldat          TYPE mkpf-bldat,
    matnr_maktx    TYPE char64,
    werks_name1    TYPE char64,
    lgort_lgobe    TYPE char64,
    menge          TYPE mseg-menge,
    meins          TYPE mseg-meins,
    valor_unitario TYPE mseg-dmbtr,
    dmbtr          TYPE mseg-dmbtr,
    flag           TYPE flag,
  END OF ty_saida.

DATA: ti_mkpf       TYPE TABLE OF ty_mkpf,
      ti_mseg       TYPE TABLE OF ty_mseg,
      ti_makt       TYPE TABLE OF ty_makt,
      ti_t001w      TYPE TABLE OF ty_t001w,
      ti_t001l      TYPE TABLE OF ty_t001l,
      ti_saida      TYPE TABLE OF ty_saida,
      ti_sort       TYPE TABLE OF slis_sortinfo_alv,
      ti_listheader TYPE TABLE OF slis_listheader,
      ti_fieldcat   TYPE TABLE OF slis_fieldcat_alv.

DATA: wa_saida      TYPE ty_saida.

##no_text
CONSTANTS: c_wl                           TYPE char2         VALUE 'WL',
           c_e                            TYPE char1         VALUE 'E',
           c_s                            TYPE char1         VALUE 'S',
           c_x                            TYPE char1         VALUE 'X',
           c_barra                        TYPE char1         VALUE '/',
           c_doispontos                   TYPE char1         VALUE ':',
           c_separador                    TYPE char1         VALUE '-',
           c_separador2                   TYPE char1         VALUE '|',
           c_separador3                   TYPE char1         VALUE ';',
           c_2008                         TYPE char4         VALUE '2008',
           c_flag                         TYPE char4         VALUE 'FLAG',
           c_mblnr                        TYPE char5         VALUE 'MBLNR',
           c_mjahr                        TYPE char5         VALUE 'MJAHR',
           c_zeile                        TYPE char5         VALUE 'ZEILE',
           c_bwart                        TYPE char5         VALUE 'BWART',
           c_bldat                        TYPE char5         VALUE 'BLDAT',
           c_matnr_maktx                  TYPE char11        VALUE 'MATNR_MAKTX',
           c_werks_name1                  TYPE char11        VALUE 'WERKS_NAME1',
           c_lgort_lgobe                  TYPE char11        VALUE 'LGORT_LGOBE',
           c_menge                        TYPE char5         VALUE 'MENGE',
           c_meins                        TYPE char5         VALUE 'MEINS',
           c_valor_unitario               TYPE char14        VALUE 'VALOR_UNITARIO',
           c_dmbtr                        TYPE char5         VALUE 'DMBTR',
           c_ti_saida                     TYPE char8         VALUE 'TI_SAIDA',
           c_mseg                         TYPE char4         VALUE 'MSEG',
           c_mkpf                         TYPE char4         VALUE 'MKPF',
           c_material                     TYPE char8         VALUE 'Material',
           c_centro                       TYPE char6         VALUE 'Centro',
           c_deposito                     TYPE char8         VALUE 'Deposito',
           c_zf_top_of_page               TYPE slis_formname VALUE 'ZF_TOP_OF_PAGE',
           c_zf_status                    TYPE slis_formname VALUE 'ZF_STATUS',
           c_zf_user_command              TYPE slis_formname VALUE 'ZF_USER_COMMAND',
           c_exp_txt                      TYPE char7         VALUE 'EXP_TXT',
           c_exp_csv                      TYPE char7         VALUE 'EXP_CSV',
           c_n_documento_de_material      TYPE char24        VALUE 'N° documento de material',
           c_ano_do_documento_do_material TYPE char28        VALUE 'Ano do documento do material',
           c_item_no_doc_do_material      TYPE char24        VALUE 'Item no doc. do material',
           c_tipo_de_movimento            TYPE char17        VALUE 'Tipo de movimento',
           c_data_no_documento            TYPE char17        VALUE 'Data no documento',
           c_quantidade                   TYPE char10        VALUE 'Quantidade',
           c_unidade_de_medida_basica     TYPE char24        VALUE 'Unidade de medida basica',
           c_valor_unitario2              TYPE char14        VALUE 'Valor unitario',
           c_montante_em_moeda_interna    TYPE char25        VALUE 'Montante em moeda interna',
           c_nome_doc_csv                 TYPE char25        VALUE 'Partidas de clientes.CSV',
           c_nome_doc_txt                 TYPE char25        VALUE 'Partidas de clientes.TXT',
           c_mbn                          TYPE char3         VALUE 'MBN',
           c_mja                          TYPE char3         VALUE 'MJA',
           c_asc                          TYPE char10        VALUE 'ASC',
           c_mb1b                         TYPE char4         VALUE 'MB1B',
           c_standard_fullscreen          TYPE char19        VALUE 'STANDARD_FULLSCREEN'.


*$*$ -------------------------------------------------------------- *$*$
*$*$                            Tela                                *$*$
*$*$ -------------------------------------------------------------- *$*$

SELECTION-SCREEN:BEGIN OF BLOCK b1 WITH FRAME TITLE text-001. " Tela de seleção
SELECT-OPTIONS: s_mblnr FOR  mkpf-mblnr.
PARAMETERS:     p_mjahr TYPE mkpf-mjahr DEFAULT c_2008.
SELECT-OPTIONS: s_bwart FOR  mseg-bwart.
SELECTION-SCREEN: END OF BLOCK b1.

*$*$ -------------------------------------------------------------- *$*$
*$*$                           Eventos                              *$*$
*$*$ -------------------------------------------------------------- *$*$

START-OF-SELECTION.
  PERFORM: zf_seleciona_dados.

END-OF-SELECTION.
  PERFORM: zf_processa_dados,
           zf_monta_alv,
           zf_quebra_de_campo,
           zf_mostra_alv.

*$*$ -------------------------------------------------------------- *$*$
*$*$                            Forms                               *$*$
*$*$ -------------------------------------------------------------- *$*$

FORM zf_seleciona_dados.

  FREE: ti_mkpf.
  SELECT mblnr
         mjahr
         bldat
    FROM mkpf
    INTO TABLE ti_mkpf
   WHERE mblnr IN s_mblnr
     AND mjahr = p_mjahr
     AND blart = c_wl.

  IF sy-subrc <> 0.
    FREE: ti_mkpf.
    MESSAGE text-e01 TYPE c_e DISPLAY LIKE c_s. " Dados do documento do material não encontrados!
    LEAVE LIST-PROCESSING.
  ENDIF.

  FREE: ti_mseg.
  SELECT mblnr
         mjahr
         zeile
         bwart
         matnr
         werks
         lgort
         dmbtr
         menge
         meins
    FROM mseg
    INTO TABLE ti_mseg
     FOR ALL ENTRIES IN ti_mkpf
   WHERE mblnr = ti_mkpf-mblnr
     AND mjahr = ti_mkpf-mjahr
     AND bwart IN s_bwart.

  IF sy-subrc <> 0.
    FREE: ti_mseg.
  ENDIF.

  IF ti_mseg IS NOT INITIAL.

    DATA(ti_mseg_aux) = ti_mseg.
    SORT ti_mseg_aux BY matnr.
    DELETE ADJACENT DUPLICATES FROM ti_mseg_aux COMPARING matnr.

    FREE: ti_makt.
    SELECT matnr
           spras
           maktx
      FROM makt
      INTO TABLE ti_makt
       FOR ALL ENTRIES IN ti_mseg_aux
     WHERE matnr = ti_mseg_aux-matnr
       AND spras = sy-langu.

    IF sy-subrc <> 0.
      FREE: ti_makt.
    ENDIF.

    FREE: ti_t001w.
    SELECT werks
           name1
      FROM t001w
      INTO TABLE ti_t001w
       FOR ALL ENTRIES IN ti_mseg
     WHERE werks = ti_mseg-werks.

    IF sy-subrc <> 0.
      FREE: ti_t001w.
    ENDIF.

    FREE: ti_t001l.
    SELECT werks
           lgort
           lgobe
      FROM t001l
      INTO TABLE ti_t001l
       FOR ALL ENTRIES IN ti_mseg
     WHERE werks = ti_mseg-werks
       AND lgort = ti_mseg-lgort.

    IF sy-subrc <> 0.
      FREE: ti_t001l.
    ENDIF.
  ENDIF. "if ti_mseg is not initial
ENDFORM. "zf_seleciona_dados.

FORM zf_processa_dados.

  DATA: wa_mkpf  TYPE ty_mkpf,
        wa_mseg  TYPE ty_mseg,
        wa_makt  TYPE ty_makt,
        wa_t001w TYPE ty_t001w,
        wa_t001l TYPE ty_t001l.

  SORT: ti_mseg  BY mblnr
                    mjahr,
        ti_mkpf  BY mblnr
                    mjahr,
        ti_makt  BY matnr,
        ti_t001w BY werks,
        ti_t001l BY werks
                    lgort.

  LOOP AT ti_mseg INTO wa_mseg.

    CLEAR wa_mkpf.
    READ TABLE ti_mkpf INTO wa_mkpf WITH KEY
                                             mblnr = wa_mseg-mblnr
                                             mjahr = wa_mseg-mjahr BINARY SEARCH.

    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    CLEAR wa_makt.
    READ TABLE ti_makt INTO wa_makt WITH KEY
                                             matnr = wa_mseg-matnr BINARY SEARCH.

    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    READ TABLE ti_t001w INTO wa_t001w WITH KEY
                                               werks = wa_mseg-werks BINARY SEARCH.

    IF sy-subrc <> 0.
      CLEAR wa_t001w.
    ENDIF.

    READ TABLE ti_t001l INTO wa_t001l WITH KEY
                                               werks = wa_mseg-werks
                                               lgort = wa_mseg-lgort BINARY SEARCH.

    IF sy-subrc <> 0.
      CLEAR wa_t001l.
    ENDIF.

    wa_saida-mblnr          = wa_mkpf-mblnr.
    wa_saida-mjahr          = wa_mkpf-mjahr.
    wa_saida-zeile          = wa_mseg-zeile.
    wa_saida-bwart          = wa_mseg-bwart.
    wa_saida-bldat          = wa_mkpf-bldat.

    CONCATENATE wa_mseg-matnr
                wa_makt-maktx
           INTO wa_saida-matnr_maktx "material
   SEPARATED BY c_separador.

    CONCATENATE wa_mseg-werks
                wa_t001w-name1
           INTO wa_saida-werks_name1 "centro
   SEPARATED BY c_separador.

    CONCATENATE wa_mseg-lgort
                wa_t001l-lgobe
           INTO wa_saida-lgort_lgobe " dep�sito
   SEPARATED BY c_separador.

    wa_saida-menge = wa_mseg-menge.
    wa_saida-meins = wa_mseg-meins.
    wa_saida-dmbtr = wa_mseg-dmbtr.

    APPEND wa_saida TO ti_saida.
    CLEAR wa_saida.

  ENDLOOP.

  LOOP AT ti_saida INTO wa_saida.
    DATA(l_tabix) = sy-tabix.

    wa_saida-valor_unitario = wa_saida-dmbtr / wa_saida-menge.
    MODIFY ti_saida FROM wa_saida INDEX l_tabix.

  ENDLOOP.

ENDFORM. "zf_processa_dados.

FORM zf_monta_alv.

  PERFORM zf_monta_fieldcat USING:
  c_mblnr             c_ti_saida   c_mblnr      c_mkpf    c_x  ''    '',
  c_mjahr             c_ti_saida   c_mjahr      c_mkpf    ''   ''    '',
  c_zeile             c_ti_saida   c_zeile      c_mseg    ''   ''    '',
  c_bwart             c_ti_saida   c_bwart      c_mseg    ''   ''    '',
  c_bldat             c_ti_saida   c_bldat      c_mkpf    ''   ''    '',
  c_matnr_maktx       c_ti_saida   ''           ''        ''   ''    c_material,
  c_werks_name1       c_ti_saida   ''           ''        ''   ''    c_centro,
  c_lgort_lgobe       c_ti_saida   ''           ''        ''   ''    c_deposito,
  c_menge             c_ti_saida   c_menge      c_mseg    ''   c_x   '',
  c_meins             c_ti_saida   c_meins      c_mseg    ''   ''    '',
  c_valor_unitario    c_ti_saida   c_dmbtr      c_mseg    ''   c_x   '',
  c_dmbtr             c_ti_saida   c_dmbtr      c_mseg    ''   c_x   ''.
ENDFORM. "zf_monta_alv


FORM zf_monta_fieldcat USING field        TYPE any
                             tab          TYPE any
                             ref_field    TYPE any
                             ref_tab      TYPE any
                             hotspot      TYPE any
                             sum          TYPE any
                             reptext_ddic TYPE any.

  DATA: wa_fieldcat   TYPE slis_fieldcat_alv.

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

  DATA: wa_layout TYPE slis_layout_alv.

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
    MESSAGE text-e02 TYPE c_s DISPLAY LIKE c_e. " Erro ao exibir o relatorio!
    LEAVE LIST-PROCESSING.
  ENDIF.


ENDFORM. "zf_mostra_alv

FORM zf_quebra_de_campo.

  DATA wa_sort TYPE slis_sortinfo_alv.

  FREE ti_sort.

  CLEAR wa_sort.
  wa_sort-spos      = 1.
  wa_sort-fieldname = c_bwart.
  wa_sort-tabname   = c_ti_saida.
  wa_sort-up        = abap_true.
  wa_sort-subtot    = abap_true.
  APPEND wa_sort TO ti_sort.

  CLEAR wa_sort.
  wa_sort-spos      = 2.
  wa_sort-fieldname = c_bldat.
  wa_sort-tabname   = c_ti_saida.
  wa_sort-up        = abap_true.
  wa_sort-subtot    = abap_true.
  APPEND wa_sort TO ti_sort.

ENDFORM. "form zf_quebra_de_campo

##called
FORM zf_top_of_page.

  DATA: wa_listheader TYPE slis_listheader.

  DATA: data      TYPE char10,
        hora      TYPE char5,
        timestamp TYPE char30.

  FREE ti_listheader.

  CONCATENATE sy-datum+6(2)
              sy-datum+4(2)
              sy-datum+0(4)
             INTO  data SEPARATED BY c_barra.

  CONCATENATE sy-uzeit+0(2)
              sy-uzeit+2(2)
             INTO hora SEPARATED BY c_doispontos.

  CONCATENATE sy-repid c_separador2 data hora INTO timestamp SEPARATED BY space.

  CLEAR wa_listheader.
  wa_listheader-typ  = c_s.
  wa_listheader-info = timestamp.
  APPEND wa_listheader TO ti_listheader.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = ti_listheader.


ENDFORM. "zf_top_of_page

##called
##needed
FORM zf_status USING pf_tab TYPE slis_t_extab.
  SET PF-STATUS c_standard_fullscreen.
ENDFORM. "zf_status

##called
FORM zf_user_command USING vl_ucomm    LIKE sy-ucomm
                          rs_selfield TYPE slis_selfield.

  CASE vl_ucomm.
    WHEN c_exp_txt.

      PERFORM zf_exporta_txt.

    WHEN c_exp_csv.

      PERFORM zf_exporta_csv.

    WHEN OTHERS.

      SET PARAMETER ID c_mbn FIELD rs_selfield-value.
      SET PARAMETER ID c_mja FIELD s_bwart.
      CALL TRANSACTION c_mb1b.

  ENDCASE.

ENDFORM. "z_user_command

FORM zf_exporta_txt.

  DATA: ti_saida_aux TYPE TABLE OF ty_saida,
        lv_caminho   TYPE string,
        vl_arq       TYPE rlgrap-filename.

  PERFORM zf_seleciona_caminho CHANGING vl_arq.

  ti_saida_aux = ti_saida.

  CONCATENATE vl_arq c_nome_doc_txt
  INTO  lv_caminho.


  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = lv_caminho
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
    MESSAGE text-e03 TYPE c_s DISPLAY LIKE c_e. " Erro ao exportar o arquivo de texto
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM. "zf_exporta_txt


FORM zf_exporta_csv.

  ##needed
  DATA: ti_saida_csv      TYPE TABLE OF string,
        wa_saida_csv      TYPE string,
        lv_dmbtr          TYPE string,
        lv_caminho        TYPE string,
        vl_menge          TYPE string,
        vl_valor_unitario TYPE string,
        vl_dmbtr          TYPE string,
        vl_arq            TYPE rlgrap-filename.

  PERFORM zf_seleciona_caminho CHANGING vl_arq.

  CLEAR wa_saida_csv.
  CONCATENATE c_n_documento_de_material
              c_ano_do_documento_do_material
              c_item_no_doc_do_material
              c_tipo_de_movimento
              c_data_no_documento
              c_material
              c_centro
              c_deposito
              c_quantidade
              c_unidade_de_medida_basica
              c_valor_unitario2
              c_montante_em_moeda_interna

  INTO wa_saida_csv
  SEPARATED BY c_separador3.

  APPEND wa_saida_csv TO ti_saida_csv.

  LOOP AT ti_saida INTO wa_saida.

    lv_dmbtr = wa_saida-dmbtr.

    vl_menge = wa_saida-menge.
    vl_valor_unitario = wa_saida-valor_unitario.
    vl_dmbtr = wa_saida-dmbtr.

    CLEAR wa_saida_csv.
    CONCATENATE wa_saida-mblnr
                wa_saida-mjahr
                wa_saida-zeile
                wa_saida-bwart
                wa_saida-bldat
                wa_saida-matnr_maktx
                wa_saida-werks_name1
                wa_saida-lgort_lgobe
                vl_menge
                wa_saida-meins
                vl_valor_unitario
                vl_dmbtr
    INTO wa_saida_csv
    SEPARATED BY ';'.

    APPEND wa_saida_csv TO ti_saida_csv.

  ENDLOOP.

  CONCATENATE vl_arq c_nome_doc_csv
  INTO  lv_caminho.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = lv_caminho
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
    MESSAGE text-004 TYPE c_s DISPLAY LIKE c_e.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM. "zf_exporta_csv

FORM zf_seleciona_caminho  CHANGING  p_arq TYPE rlgrap-filename.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    CHANGING
      file_name     = p_arq
    EXCEPTIONS
      mask_too_long = 1
      OTHERS        = 2.

  IF sy-subrc <> 0.
    MESSAGE text-005 TYPE c_s DISPLAY LIKE c_e.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.