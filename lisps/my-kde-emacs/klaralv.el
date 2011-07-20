;; ------------------------------ COPYRIGHT NOTICE ------------------------------
;; klaralv.el version 1.4
;; Copyright Klaralvdalens Datakonsult AB.
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 51 Franklin Street, Fifth Floor., Boston, MA 02110-1301, USA.


;; ------------------------------ INSTALLATION ------------------------------
;; To use this file, put this file in your emacs config directory.
;; If you do not have such a directory, create one and add 
;; something like the following to your .emacs: 
;; (setq load-path (cons "/home/joe/Emacs/" load-path))
;;
;; Next insert the following line into your .emacs
;; (require 'klaralv)
;; (global-set-key [(f5)] 'kdab-insert-header)
;; (global-set-key [(shift f5)] 'kdab-insert-forward-decl)
;; (global-set-key [(control f5)] 'kdab-lookup-qt-documentation)
;; 
;; If you use Qtopia, insert the following line to activate Qtopia headers
;; (setq kdab-include-qpe 't)
;; If you do not want include files to be prefixed with qtopia/,
;; as in qtopia/qpeapplication, then insert the following code in your setup
;; (setq kdab-prefix-qpe nil)
;;
;; If you are using Qt 4 rather than Qt 3, insert the following line into you setup:
;; (setq kdab-qt-version 4)
;; or in a .emacs-dirvars file:
;; kdab-qt-version: 4
;;
;; Finally, if you do not want header files to be inserted lower-cased when
;; not found locally, insert (kdab-lowercase-header-files nil)

;; ------------------------------ CONFIGURATION ------------------------------
(defvar kdab-qt-version 3
  "Specify which header files to use")

(defvar kdab-include-qpe nil
  "Specifies whether Qtopia headers should be included")

(defvar kdab-prefix-qpe 't
  "set this to nil if you do not want QTopia header files prefixed with qtopia/")

(defvar kdab-lowercase-header-files 't
  "Should header files be all lowercase, or would you prefer to use same case as class?")

;; ------------------------------ Include Specifications ------------------------------ 
;; special case for include files
;; Please notify blackie@klaralvdalens-datakonsult.se with any modification to this variable!
(defvar kdab-special-includes
  '( 
    ; KDE
    (kdebug.h kDebug kWarning kError kFatal kBacktrace kdDebug kdWarning kdError kdFatal kdBacktrace)
    (kicondialog.h KIconCanvas KIconButton)
    (knuminput.h KDoubleNumInput KIntNumInput)

    ; KDGear - http://www.klaralvdalens-datakonsult.se
    (KDCheckableGroupBox.h KDCheckableGroupBox)
    (KDCheckableHGroupBox.h KDCheckableHGroupBox)
    (KDCheckableVGroupBox.h KDCheckableVGroupBox)
    (KDCloseableWidget.h KDCloseableWidget)
    (KDConfigDialog.h KDConfigDialog)
    (KDConfigWidget.h KDConfigWidget)
    (KDDateWidget.h KDDateWidget KDDateTimeWidget)
    (KDDirMonitor.h KDDirMonitor)
    (KDGridWidget.h KDGridWidget)
    (KDListBoxPair.h KDListBoxPair)
    (KDMinimizeSplitter.h KDMinimizeSplitter)
    (KDSearchableListBox.h KDSearchableListBox)
    (KDSemiSizingControl.h KDSemiSizingControl)
    (KDShowHideTableControl.h KDShowHideTableControl)
    (KDSimpleSizingControl.h KDSimpleSizingControl)
    (KDSizingControl.h KDSizingControl)
    (KDStream.h KDStream)
    (KDTimeWidget.h KDTimeWidget)

    ; KDChart - http://www.klaralvdalens-datakonsult.se
    (KDChart.h KDChart)
    (KDChartAxisParams.h KDChartAxisParams)
    (KDChartBaseSeries.h KDChartBaseSeries)
    (KDChartCustomBox.h KDChartCustomBox)
    (KDChartData.h KDChartData)
    (KDChartEnums.h KDChartEnums)
    (KDChartListTable.h KDChartListTableData KDChartListTablePrivate)
    (KDChartNotEnoughSpaceException.h KDChartNotEnoughSpaceException)
    (KDChartPainter.h KDChartPainter)
    (KDChartParams.h KDChartFrameSettings KDChartParams ModeAndChart)
    (KDChartPlaneSeries.h KDChartPlaneSeries)
    (KDChartPropertySet.h KDChartPropertySet)
    (KDChartSeriesCollection.h  KDChartSeriesCollection)
    (KDChartTable.h KDChartTableData)
    (KDChartTableBase.h KDChartTableDataBase)
    (KDChartTextPiece.h KDChartTextPiece)
    (KDChartUnknownTypeException.h KDChartUnknownTypeException)
    (KDChartVectorSeries.h KDChartVectorSeries)
    (KDChartVectorTable.h KDChartVectorTableData KDChartVectorTablePrivate)
    (KDChartWidget.h KDChartWidget)
    (KDFrame.h KDFrame KDFrameCorner)
    (KDFrameProfileSection.h KDFrameProfileSection)

    ; Useful fake entries
    (kapplication.h kapp)
    (klocale.h i18n I18N_NOOP)
    (kstandarddirs.h locate locateLocal)
    (stdlib.h getenv)
    (unistd.h unlink sleep usleep)
    (iostream cout cerr)
    (ctype.h isalnum isalpha isascii isblank iscntrl isdigit isgraph islower isprint ispunct isspace isupper isxdigit)
    )
    "List of special include files which do not follow the normal scheme")

(defvar kdab-qt3-special-includes
  '((qlayout.h QHBoxLayout QVBoxLayout QGridLayout QBoxLayout)
    (qlistview.h QListViewItem QCheckListItem QListViewItemIterator)
    (qiconview.h QIconViewItem QIconDragItem QIconDrag)
    (qdragobject.h QTextDrag QStoredDrag QUriDag QColorDrag QImageDrag QDragManager)
    (qmime.h QMimeSource QMimeSourceFactory QWindowsMime)
    (qptrlist.h QPtrListIterator)
    (qevent.h QTimerEvent QMouseEvent QWheelEvent QTabletEvent QKeyEvent 
              QFocusEvent QPaintEvent QMoveEvent QResizeEvent QCloseEvent 
              QShowEvent QHideEvent QContextMenuEvent QIMEvent QDropEvent
              QDragMoveEvent QDragEnterEvent QDragResponseEvent QDragLeaveEvent
              QChildEvent QCustomEvent)
    (qdatetime.h QTime QDateTime QDate)
    (qdatetimeedit.h QTimeEdit QDateTimeEditBase QDateEdit QDateTimeEdit)
    (qcstring.h QByteArray)
    (qobjectlist.h QObjectListIt QObjectListIterator)
    (qwidgetlist.h QWidgetListIt)
    (qtabbar.h QTab)
    (qpalette.h QColorGroup)
    (qaction.h QActionGroup)
    (qvalidator.h QIntValidator QDoubleValidator QRegExpValidator)
    (qlistbox.h QListBoxItem QListBoxText QListBoxPixmap)
    (qstring.h QChar QCharRef QConstString)
    (qcanvas.h QCanvasSprite QCanvasPolygonalItem QCanvasRectangle
               QCanvasPolygon QCanvasEllipse QCanvasText QCanvasLine
               QCanvasChunk QCanvas QCanvasItem QCanvasView QCanvasPixmap)
    (qgl.h QGLFormat QGL QGLContext QGLWidget QGLColormap)
    (qtable.h QTableSelection QTableItem QComboTableItem QCheckTableItem) 
    (qsqlfield.h QSqlField QSqlFieldInfo)
    (qsqlrecord.h QSqlRecord QSqlRecordInfo)
    
    ; Qt/Embedded
    (qcopchannel_qws.h QCopChannel)
    (qdirectpainter_qws.h QDirectPainter)
    (qfontfactorybdf_qws.h QFontFactoryBDF)
    (qfontfactoryttf_qws.h QFontFactoryFT)
    (qfontmanager_qws.h QGlyphMetrics QGlyph QRenderedFont QDiskFont QFontManager QFontFactory)
    (qgfx_qws.h QScreenCursor QPoolEntry QScreen QGfx)
    (qgfxlinuxfb_qws.h QLinuxFbScreen)
    (qgfxmatroxdefs_qws.h QQnxFbGfx QQnxScreen)
    (qgfxraster_qws.h QGfxRasterBase QGfxRaster)
    (qgfxvnc_qws.h QRfbRect QRfbPixelFormat QRfbServerInit QRfbSetEncodings 
                   QRfbFrameBufferUpdateRequest QRfbKeyEvent QRfbPointerEvent QRfbClientCutText QVNCServer)
    (qkeyboard_qws.h QWSKeyboardHandler)
    (qlock_qws.h QLock QLockHolder)
    (qmemorymanager_qws.h QMemoryManagerPixmap QMemoryManager)
    (qsoundqss_qws.h QWSSoundServer QWSSoundClient QWSSoundServerClient QWSSoundServerSocket)
    (qwindowsystem_qws.h QWSInternalWindowInfo QWSScreenSaver QWSWindow QWSSoundServer 
                         QWSServer qwsServer KeyboardFilter QWSClient)
    (qwsbeosdecoration_qws.h QWSBeOSDecoration)
    (qwscursor_qws.h QWSCursor)
    (qwsdecoration_qws.h QWSDecoration)
    (qwsdefaultdecoration_qws.h QWSDefaultDecoration)
    (qwsdisplay_qws.h QWSWindowInfo QWSDisplay)
    (qwshydrodecoration_qws.h QWSHydroDecoration)
    (qwskde2decoration_qws.h QWSKDE2Decoration)
    (qwskdedecoration_qws.h QWSKDEDecoration)
    (qwsmanager_qws.h QWSManager QWSButton)
    (qwsmouse_qws.h QWSPointerCalibrationData QWSMouseHandler QCalibratedMouseHandler 
                    QAutoMouseHandlerPrivate QWSMouseHandlerPrivate QVrTPanelHandlerPrivate 
                    QTPanelHandlerPrivate QYopyTPanelHandlerPrivate QCustomTPanelHandlerPrivate 
                    QVFbMouseHandlerPrivate)
    (qwsproperty_qws.h QWSPropertyManager)
    (qwsregionmanager_qws.h QWSRegionManager)
    (qwssocket_qws.h QWSSocket QWSServerSocket)
    (qwswindowsdecoration_qws.h QWSWindowsDecoration)
    (qstatusbar.h statusBar())

    ; Useful fake entries
    (qapplication.h qApp)
    (qglobal.h qDebug qWarning)
    (qeventloop.h eventloop)    
  ))

(defvar kdab-qt4-special-includes
  '(
    ; Useful fake entries
    (QApplication qApp)
    (QDebug qDebug qWarning)
    (QEventLoop eventloop)
    ))
  
(defvar kdab-qpe-includes 
  '(
    (alarmserver.h AlarmServer)
    (applnk.h AppLnk DocLnk AppLnkSet DocLnkSet)
    (calendar.h Calendar)
    (categories.h CategoryGroup CategoryGroup Categories CheckedListView)
    (categorymenu.h CategoryMenu)
    (categoryselect.h CategoryCombo CategorySelect CategoryEdit CategoryWidget)
    (config.h Config)
    (contact.h Contact)
    (database.h QWSDatabase DatabaseDefaultView Database DatabaseView DatabaseDefaultView)
    (datebookdb.h DateBookDB)
    (datebookmonth.h DateBookMonthHeader DayItemMonth DateBookMonthTable DateBookMonth DateButton)
    (event.h Event EffectiveEvent EffectiveEventSizeSorter EffectiveEventTimeSorter)
    (filemanager.h FileManager)
    (fileselector.h FileSelectorItem FileSelector)
    (finddialog.h FindDialog)
    (fontdatabase.h FontDatabase)
    (fontmanager.h FontManager)
    (global.h Global)
    (imageedit.h ImageEdit)
    (inputmethodinterface.h InputMethodInterface)
    (ir.h Ir)
    (lightstyle.h LightStyle)
    (lnkproperties.h LnkProperties)
    (mediaplayerplugininterface.h MediaPlayerDecoder)
    (menubutton.h MenuButton)
    (mimetype.h MimeType)
    (network.h Network)
    (palmtoprecord.h Record)
    (palmtopuidgen.h UidGen)
    (password.h Password)
    (power.h PowerStatus PowerStatusManager )
    (process.h Process)
    (qcopenvelope_qws.h QCopEnvelope)
    (qdawg.h QDawg)
    (qlibrary.h QLibrary)
    (qpeapplication.h QPEApplication)
    (qpedecoration_qws.h QPEDecoration QPEManager)
    (qpedialog.h QPEDialogListener)
    (qpemenubar.h  QPEMenuToolFocusManager QPEMenuBar)
    (qpemessagebox.h QPEMessageBox)
    (qpestyle.h QPEStyle : public QWindowsStyle)
    (qpetoolbar.h QPEToolBar)
    (record.h Record)
    (resource.h Resource)
    (sound.h Sound)
    (storage.h StorageInfo FileSystem)
    (task.h Task)
    (timeconversion.h TimeConversion)
    (timestring.h DateFormat TimeString)
    (tzselect.h TZCombo TimeZoneSelector)
    ))

(defvar kdab-qt4-classes
  '(QtNsPlugin QClipboard QPixmap QPen QRgb QMessageBox
               QRegion QImageReader QItemEditorCreator QTimeEdit QCloseEvent QPushButton
               QAbstractScrollArea QTabBar QSlider QPicture QRadialGradient QStandardItemModel QStringListModel
               QListWidget QPageSetupDialog QAccessibleApplication QHelpEvent QTextDocumentFragment QDragMoveEvent
               QImageIOHandlerFactoryInterface QDropEvent QInputContextFactoryInterface QStyleHintReturnMask QWidget QTextTableCell
               QPrintDialog QKeyEvent QDial QDrag QFont QIcon
               QItemSelection QMenu QImageWriter QAbstractSlider QtGui QStyleOptionToolBox
               QTableView QWidgetData QPrintEngine QWidgetItem QWidgetList QStyleOptionProgressBar
               QScrollBar QPolygon QAccessibleFactoryInterface QProgressDialog QStyleOptionButton QClipboardEvent
               QHideEvent QDragLeaveEvent QWhatsThisClickedEvent QColor QLineEdit QStyleOptionFocusRect
               QFrame QImage QPrinter QLabel QMovie QStyleOptionTabWidgetFrame
               QSound QStyle QMoveEvent QAccessibleBridge QStyleOptionHeader QFileOpenEvent
               QGradientStop QMenuItem QAbstractPageSetupDialog QVBoxLayout QItemEditorFactory QTextObjectInterface
               QIconEngineFactoryInterface QTreeView QStyleOptionTabBarBase QWhatsThisAction QListView QGradientStops
               QRegExpValidator QAbstractSpinBox QTextFragment QStackedLayout QAccessibleInterface QCommonStyle
               QIconEngine QDateEdit QLayoutIterator QPictureFormatInterface QTextTableFormat QResizeEvent
               QFontMetrics QGradient QImageIOHandler QShortcut QAccessibleObject QTextBlockGroup
               QStyleOptionSlider QApplication QTableWidgetItem QInputDialog QPainterPathStroker QVBoxWidget
               QAccessiblePlugin QTextBrowser QStyleFactoryInterface QImageTextKeyLang QErrorMessage QTextFrameLayoutData
               QHoverEvent QX11Info QScrollArea QDirModel QAbstractTextDocumentLayout QWindowsStyle
               QStackedWidget QStyleOptionTitleBar QAbstractItemView QStylePainter QSplitter QStyleOptionViewItem
               QAccessibleWidget QVFbHeader QAccessible QTextFrameFormat QGridLayout QButtonGroup
               QFontInfo QMotifPlusStyle QProgressBar QLinearGradient QMacMime QContextMenuEvent
               QStyleHintReturn QStyleOptionQ3ListViewItem QActionEvent QActionGroup QDockWidget QWidgetContainerPlugin
               QWSUpdateEvent QStyleOptionMenuItem QVFbKeyData QAbstractItemDelegate QPolygonF QCheckBox
               QIntValidator QColorGroup QTreeWidgetItem QGroupBox QCursorShape QTableWidgetSelectionRange
               QCompactStyle QSplitterHandle QMenuBar QStyleOptionDockWidget QWindowsXPStyle QPixmapCache
               QSpinBox QDoubleSpinBox QTextCursor QFontMetricsF QInputMethodEvent QShortcutEvent
               QSizeGrip QInputContextFactory QLCDNumber QTextEdit QTextItem QTextLine
               QTextList QToolButton QGridWidget QFontDatabase QTextFormat QListWidgetItem
               QPlatinumStyle QAccessibleBridgeFactoryInterface QKeySequence QStyleOptionComplex QTableWidget QFontDialog
               QTextInlineObject QStyleOptionFrame QComboBox QTextListFormat QHBoxLayout QColormap
               QDragEnterEvent QStyleOptionQ3ListView QDragResponseEvent QValidator QStyleOptionToolButton QItemDelegate
               QStyleOptionQ3DockWindow QTextDocument QWMatrix QTextLayout QTreeWidget QRadioButton
               QSplashScreen QToolBarChangeEvent QTextCharFormat QToolBar QToolBox QToolTip
               QDesktopWidget QDoubleValidator QShowEvent QTextObject QConicalGradient QTextOption
               QDateTimeEdit QWidgetMapper QWindowsMime QStyleOption QLayoutItem QTextBlock
               QTextImageFormat QTextFrame QPaintDevice QSizePolicy QTextTable QFocusEvent
               QAction QFocusFrame QTextBlockFormat QHBoxWidget QHeaderView QPaintEngine
               QWidgetPlugin QMainWindow QBitmap QAbstractUndoItem QSpacerItem QFileIconProvider
               QColorDialog QDialog QDialogButtonBox QCDEStyle QCursor QTabWidget QItemSelectionModel
               QFileDialog QInputEvent QItemSelectionRange QProxyModel QRubberBand QItemEditorCreatorBase
               QMotifStyle QPainterPathPrivate QStatusTipEvent QAbstractButton QPictureIO QWheelEvent
               QAbstractPrintDialog QBrushData QInputContext QMouseEvent QBoxLayout QLayout
               QPainter QPalette QMatrix QIconSet QWidgetFactoryInterface QStyleOptionComboBox
               QSessionManager QStyleOptionSpinBox QTabletEvent QStyleOptionTab QStatusBar QWhatsThis
               QPaintEvent QStyleFactory QSGIStyle QIconDragEvent QSqlRelationalDelegate QTDSResult
               QOCIResult QtSql QPSQLResult QSqlDriver QSqlDriverFactoryInterface QSqlTableModel
               QIBaseDriver QSqlRelation QSqlRecord QSqlResult QSqlDatabase QIBaseResult
               QSQLiteDriver QSqlDriverCreatorBase QSQLite2Driver QSqlError QSqlField QSqlIndex
               QSqlQuery QODBCDriver QSQLiteResult QDB2Driver QSQLite2Result QSqlRelationalTableModel
               QMYSQLDriver QODBCResult QSqlDriverCreator QDB2Result QSqlQueryModel QTDSDriver
               QOCIDriver QPSQLDriver QMYSQLResult QDomProcessingInstruction QXmlContentHandler QXmlParseException
               QXmlLexicalHandler QtXml QDomNodeList QDomElement QDomDocument QDomDocumentType
               QDomAttr QDomNode QDomText QDomEntity QDomNotation QXmlDefaultHandler
               QDomDocumentFragment QXmlDeclHandler QXmlAttributes QXmlEntityResolver QDomImplementation QXmlInputSource
               QXmlLocator QXmlReader QDomNamedNodeMap QXmlDTDHandler QXmlErrorHandler QDomCharacterData
               QDomEntityReference QXmlNamespaceSupport QXmlSimpleReader QDomCDATASection QDomComment QTS
               QDir QMap QSet QUrl QPointF QSharedDataPointer
               QStringMatcher QVariantMap QRegExp QVectorData QIODevice QReadLocker
               QVariantComparisonHelper QSignal QThread QString QLatin1String QVector
               QSingleCleanupHandler QtPluginInstanceFunction QBool QChar QDate QFile
               QFlag QLine QList QPair QRect QSize
               QTime QUuid Q_PID QPointer QAbstractEventDispatcher QCharRef
               QStringListIterator QAbstractListModel QChildEvent QMutableSetIterator QFactoryInterface QGlobalStatic
               Q_INT16 Q_INT32 Q_INT64 Q_LLONG QCache QDebug
               QEvent QFlags Q_UINT8 Q_ULONG QMutableStringListIterator QLineF
               QMutex QProcess QPoint QSignalEmitter QRectF QQueue
               QSizeF QStack QTimer QTextCodecFactoryInterface Q_INT8 Q_LONG
               QCleanupHandler QBasicAtomicPointer QtCore QByteRef QBitArray QVariantList
               QSharedCleanupHandler QListData QMetaEnum QMetaType QModelIndexList QSocketNotifier
               QDateTime QDataStream QStringList QObjectData QObjectList QForeachContainer
               QtAlgorithms QLibraryInfo QSharedData QResource QByteArrayMatcher QMutableVectorIterator
               QTypeInfo QVectorTypedData QSemaphore QThreadStorage QMetaProperty QVectorIterator
               QTranslatorMessage QPersistentModelIndex QMutexLocker QLibrary QSignalMapper QTextDecoder
               QMetaClassInfo QSetIterator QtMsgHandler QMapData QHashIterator QFixedPoint
               QMultiMap QFileEngineHandler QHashDummyValue QBasicTimer QCoreApplication QFileInfo
               QExplicitlySharedDataPointer QTimerEvent QNoImplicitBoolCast QMutableHashIterator QMutableMapIterator QEventLoop
               QCustomEvent QGenericReturnArgument QHashData QHashNode QSysInfo QFileInfoListIterator
               QByteArray QModelIndex QWaitCondition QVarLengthArray QMetaMember QTextEncoder
               QLatin1Char QMimeData QMetaObject QForeachMemory QThreadStorageData QMetaResource
               QTemporaryFile QTextCodec QListIterator QLinkedListData QFileInfoList QLinkedListNode
               QNoDebug QReadWriteLock QTextStream QObjectCleanupHandler QFixedPointLong QtCleanUpFunction
               QAtomic QBitRef QTextStreamFunction QGenericArgument QObjectUserData QVariant
               QBuffer QLinkedList QArgument QMetaTypeId QtGlobal QTextOStream
               QMutableListIterator QFileEngine QWriteLocker QBasicAtomic QTextStreamManipulator QStdWString
               QLinkedListIterator Q_UINT16 Q_UINT32 Q_UINT64 Q_ULLONG QAbstractTableModel
               QMapIterator QAtomicPointer QLocale QMutableLinkedListIterator QConstString QAbstractItemModel
               QSettings QObject QTextIStream QReturnArgument QTranslator QPluginLoader
               ActiveQt QFtp QTcpServer QTcpSocket QHttp QAbstractSocket
               QIPv6Address QUdpSocket QHttpHeader QtNetwork QHttpResponseHeader QHttpRequestHeader
               QHostInfo Q_IPV6ADDR QUrlInfo QHostAddress QMotif QtMotif
               QMotifDialog QXtWidget QMotifWidget Q3DragObject Q3TabDialog Q3CanvasItemList
               Q3DeepCopy Q3StrIVec Q3StrList Q3IntBucket Q3DropSite Q3PtrListStdIterator
               Q3AsciiDictIterator Q3CanvasPixmapArray Q3ListBoxItem Q3ListBoxText Q3DockWindow Q3Dns
               Q3Ftp Q3Url Q3SqlPropertyMap Q3ToolBar Q3HttpRequestHeader Q3SqlRecordInfo
               Q3StringBucket Q3ImageDrag Q3Http Q3FileIconProvider Q3DockAreaLayout Q3CanvasRectangle
               Q3ServerSocket Q3TableSelection Q3CanvasItem Q3CanvasLine Q3CanvasText Q3CanvasView
               Q3StoredDrag Q3TextEditOptimPrivate Q3MultiLineEdit Q3ValueList Q3NetworkProtocolFactory Q3NetworkProtocol
               Q3DnsSocket Q3CString Q3UriDrag Q3Action Q3ListViewItemIterator Q3Canvas
               Q3IntCacheIterator Q3ValueListConstIterator Q3Button Q3GCache Q3Painter Q3TimeEdit
               Q3DataBrowser Q3StyleSheet Q3Header Q3StrIList Q3MainWindow Q3WhatsThis
               Q3NetworkOperation Q3SqlFieldInfoList Q3CanvasPolygon Q3FileDialog Q3DockArea Q3Picture
               Q3SqlSelectCursor Q3MemArray Q3PtrListIterator Q3ComboTableItem Q3SortedList Q3CanvasPolygonalItem
               Q3SocketDevice Q3HButtonGroup Q3PtrDictIterator Q3VButtonGroup Q3PolygonScanner Q3IconViewItem
               Q3AsciiCacheIterator Q3PtrBucket Q3PointArray Q3ObjectDictionary Q3Shared Q3Signal
               Q3SqlFieldInfo Q3TableItem Q3CheckTableItem Q3Socket Q3StrVec Q3SqlCursor
               Q3DateTimeEdit Q3CheckListItem Q3ListView Q3Accel Q3Process Q3Cache
               Q3GDict Q3GList Q3CanvasPixmap Q3Frame Q3LNode Q3SpinWidget
               Q3FilePreview Q3Wizard Q3Table Q3ValueVector Q3SyntaxHighlighter Q3DataView
               Q3DictIterator Q3DateEdit Q3CanvasSpline Q3CanvasSprite Q3ColorDrag Q3SimpleRichText
               Q3ValueListIterator Qt3Support Q3ValueStack Q3WidgetStack Q3NetworkProtocolDict Q3HttpResponseHeader
               Q3TextBrowser Q3ListBoxPixmap Q3GridView Q3SqlEditorFactory Q3CacheIterator Q3AsciiBucket
               Q3EditorFactory Q3CanvasEllipse Q3MimeSourceFactory Q3PopupMenu Q3PaintDeviceMetrics Q3DateTimeEditBase
               Q3HttpHeader Q3DataTable Q3ButtonGroup Q3ProgressDialog Q3Semaphore Q3ProgressBar
               Q3NetworkProtocolFactoryBase Q3IconDrag Q3IconView Q3GroupBox Q3ListViewItem Q3IntDictIterator
               Q3ActionGroup Q3ListBox Q3GVector Q3IconDragItem Q3RangeControl Q3StyleSheetItem
               Q3StrListIterator Q3TextEdit Q3TextDrag Q3LocalFs Q3TextView Q3Workspace
               Q3GListStdIterator Q3SqlForm Q3UrlOperator Q3ScrollView QGLContext QGLFormat
               QGLWidget QGLColormap QtOpenGL 
               QDesignerFormEditorInterface QDesignerCustomWidgetInterface QDesignerContainerExtension) )

;; ------------------------------ SOURCE CODE ------------------------------

;; build Qt4 special include list
(defun kdab-build-qt4-special-includes ()
  (let ( elm (res kdab-qt4-special-includes) (list kdab-qt4-classes))
    (while list
      (progn
        (setq elm (car list))
        (setq list (cdr list))
        (setq res (cons (list elm elm ) res ))))
    res))

(defun kdab-build-qpe-special-incldues ()
  (let (elm header classes (list kdab-qpe-includes) filename (result '()))
    (while list
      (setq elm (car list))
      (setq list (cdr list))
      (setq filename (concat (if kdab-prefix-qpe "qtopia/" "") (symbol-name (car elm))))
      (setq result (cons (cons (intern filename) (cdr elm)) result)))
    result))

(defun kdab-join-lists (list1 list2)
  (let ((res list1)
        (list list2)
        elm)
    (while list
      (progn
        (setq elm (car list))
        (setq list (cdr list))
        (setq res (cons elm res))))
    res))

(defun kdab-get-special-include-list ()
  (kdab-join-lists kdab-special-includes
                   (kdab-join-lists (if (eq kdab-qt-version 3) kdab-qt3-special-includes 
                                      (if (eq kdab-qt-version 4) (kdab-build-qt4-special-includes) '()))
                                    (if kdab-include-qpe (kdab-build-qpe-special-incldues) '()))))

;; Lookup class `cls' in kdab-special-includes and return the associate include file name
(defun kdab-map-special (cls)
  (let ((list (kdab-get-special-include-list))
        (found nil))
    (while (and list (not found))
      (let* ( (elm (car list))
              (include-file (car elm))
              (classes (cdr elm)))
        ( while (and classes (not found))
          (if (string= (downcase cls) (downcase (symbol-name (car classes))))
              (setq found include-file)
            (setq classes (cdr classes)))))
      (setq list (cdr list)))
    (if found
        (symbol-name found)
      nil)  ; return value
    ))
        


;--------------------------------------------------------------------------------
; Insert include file for Qt program.
; Place point anywhere on a Qt class, and invoke this function. A result of
; this is that an include line is added (if it does not already exists) for
; the given class.
;--------------------------------------------------------------------------------
(defun kdab-insert-header ( prefix )
  "Insert include file for class at point"
  (interactive "P")
  (save-excursion
    (let* ((word-at-point (if prefix
                              (read-from-minibuffer "Class: ")
                            (current-word))))
      (kdab-insert-header-non-interactive word-at-point))))

;--------------------------------------------------------------------------------
; insert include file for `word-with-case' non-interactively.
; for an interactive version see kdab-insert-header
;--------------------------------------------------------------------------------
(defun kdab-insert-header-non-interactive (word-with-case)
  (save-excursion
    (let* ((word (downcase word-with-case))
           (special-header (cond
                    ((kdab-map-special word) (kdab-map-special word))
                    ((and (string-match "^qdom" word) (eq kdab-qt-version 3)) "qdom.h")
                    ((and (string-match "^qxml" word) (eq kdab-qt-version 3)) "qxml.h")
                    ((and (string-match "^q" word) (eq kdab-qt-version 3)) (concat word ".h") )
                    (kdab-lowercase-header-files (concat word ".h" ))
                    (t (concat word-with-case ".h"))))
           header is-local override)

      
      ;; decide on the header file.
      (if (functionp 'kdab-name-include-file)
          (setq override (kdab-name-include-file word-with-case)))

      (if override
          (progn ;; Users hook solved it for us.
            (setq header (car override))
            (setq is-local (cdr override)))
        (if (file-exists-p (concat word-with-case ".h"))
            (progn ; file exists in given case in pwd.
              (setq header (concat word-with-case ".h"))
              (setq is-local 't))
          (if  (file-exists-p (concat word ".h")) ; file exists in lowercase in pwd
              (progn
                (setq header (concat word ".h"))
                (setq is-local 't))
            (if (file-exists-p word-with-case)
                (progn
                  (setq header word-with-case)
                  (setq is-local 't))
              (if (file-exists-p word)
                  (progn
                    (setq header word)
                    (setq is-local 't))
                (progn ; header in <..> path
                  (setq header special-header)
                  (setq is-local nil)))))))

      (kdab-insert-include-file header is-local t))))

;--------------------------------------------------------------------------------
; Insert header file for header. If is-local insert it with "" 
; otherwise insert it with <>
;--------------------------------------------------------------------------------
(defun kdab-insert-include-file (header is-local show-message)
  (let ((include-file (if is-local
                          (concat "#include \"" header "\"")
                        (concat "#include <" header ">"))))

    (beginning-of-buffer)
    ; first look for //#include "foo.h" being already there
    (if (re-search-forward (concat "^ *// *\\(#include *[<\"][ \t]*" header "[ \t]*[>\"]\\)") nil t)
        (progn
          (replace-match "\\1")
          (when show-message
            (message (concat "commented in #include for " header))))
      
      (if (not (re-search-forward (concat "#include *[\"<][ \t]*" header "[ \t]*[\">]") nil t))
          (progn
            ; No include existed
            (goto-char (point-max)) ; Using end-of-buffer makes point move, despite save-excursion
	    (while (and (re-search-backward "^#include *[\"<]\\([^\">]+\\)[\">]" nil t)
			(string-match ".*moc.*" (match-string 1))) 
	      ; each iteration moves up until finding a #include which isn't a moc
	      )
            (if (not (looking-at "^#include *[\"<][^\">]+ *[\">]"))
                (beginning-of-buffer)
              (progn (end-of-line) (forward-char 1)))
            
            ;; Now insert the header
            (insert (concat include-file "\n"))
            (when show-message
              (message (concat "inserted " include-file))))
        (when show-message
              (message (concat "header file \"" header "\" is already included")))))))



;----------------------------------------------------------------------------
; Insert a forward declaration for a Qt class.
; Place point anywhere on a Qt class, and invoke this function. A
; result of this is that a forward declaration line is added (if it does
; not already exist) for the given class.
;----------------------------------------------------------------------------
(defun kdab-insert-forward-decl ( prefix )
  (interactive "P")
  (save-excursion
    (let* ((word (if prefix (read-from-minibuffer "Class: ")
                   (current-word))))
      (beginning-of-buffer)
      (if (re-search-forward (concat "^ *// *\\(class *" word ";\\)") nil t)
          (progn
            (replace-match "\\1")
            (message (concat "commented in forward declaration for " word)))

        (if (not (re-search-forward (concat "class *" word ";") nil t))
            (progn
                                        ; No forward decl existed
                                        ; Look for other forward declarations and insert this one before them
                                        ; (this avoids finding class Private; inside a class, or other stuff in the middle of the file)
              (if (re-search-forward "^[ \t]*class .*;" nil t)
                  (progn
					; Exit namespace foo { class bar; } if necessary
					; This is a modified version of (backward-up-list) which doesn't
					; throw an error when not found.
		    (goto-char (or (scan-lists (point) -1 1 nil t) (point))) ; ### do multiple times if necessary
		    (re-search-backward "^[ \t]*namespace " nil t) ; in case of namespace foo\n{
		    (beginning-of-line))
                                        ; No forward declarations found, lets search for include lines.
                                        ; For those we start from the end, because we want to leave file.h first.
                (progn (goto-char (point-max))
		       (if (re-search-backward "#include" nil t)
			   (progn (end-of-line) (forward-char 1))
			 (beginning-of-buffer))))
              
              (progn
                (insert "class " word ";\n")
                (message (concat "inserted class " word ";"))))
          (message (concat "forward decl for \"" word "\" already exists")))))))
  

(defun is-qpe-class (class)
  (let ((list kdab-qpe-includes) classes (found nil))
    (while (and (not found) list)
      (setq classes (cdr (car list)))
      (while classes
        (if (string= (downcase (symbol-name (car classes))) (downcase class))
            (setq found 't))
        (setq classes (cdr classes)))
      (setq list (cdr list)))
    found))
        

(provide 'klaralv)
