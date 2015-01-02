<?php
    require_once 'safe_slash.php'; // безопасная обработка параметров
    require_once 'process.php'; // GetPage()
    require_once 'common.php'; // <--- "session_start();" inside
    require_once 'printer.php';
    require_once 'mysql.php'; // to establish connection

    $userinfo = "";
    $menu = "";
    $content = "";

    if (IsLogged())
    {
        $userinfo = BlockUserLogged(GetUserName());
        $menu = BlockMenu(SCREEN_USER, 4);
    } else
    {
        $userinfo = BlockUserAnon();
        $menu = BlockMenu(SCREEN_WELCOME, 2);
    }
    
    $content = BlockDownloads();
    echo BlockMain($userinfo, $menu, $content);
?>