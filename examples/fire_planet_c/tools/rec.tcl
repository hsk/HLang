# openMSX の画面を録画する。遊んでウィンドウを閉じると、openMSX が AVI を仕上げる
proc fire_start_recording {} {
    record start -triplesize -videoonly -prefix fire
}
after frame fire_start_recording
