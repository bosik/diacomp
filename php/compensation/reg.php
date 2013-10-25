<?php
    require_once 'printer.php';
    require_once 'safe_slash.php';
    require_once 'common.php';

    // ���� ��� ������ ������
    if (empty($_POST))
    {
        $userinfo = "";
        $menu = BlockMenu(SCREEN_AUTH, 2);
        $content = BlockRegForm("", "", "");
        echo BlockMain($userinfo, $menu, $content);
    }
    else

    // ���� ��� ������� �����������
    {
        // ���� ���� ��������� ������
        if (!isset($_POST['login']) ||
            !isset($_POST['password']) ||
            !isset($_POST['captcha']) ||
            !isset($_SESSION['captcha_keystring'])
            )
        {
            $userinfo = "";
            $menu = BlockMenu(SCREEN_AUTH, 2);
            $content = BlockRegForm("", "", "��������� �������");
            echo BlockMain($userinfo, $menu, $content);
        }

        else

        // ���� ��������� ������ ���, ��������� ����������
        {
            $newlogin = $_POST['login'];
            $newpass = $_POST['password'];
            if ($newlogin == '')
            {
                $userinfo = "";
                $menu = BlockMenu(SCREEN_AUTH, 2);
                $content = BlockRegForm("", "", "������� email");
                echo BlockMain($userinfo, $menu, $content);
            }

            else

            if (ExistsUser($newlogin))
            {
                $userinfo = "";
                $menu = BlockMenu(SCREEN_AUTH, 2);
                $content = BlockRegForm("", "", "������������ � ����� email ��� ����������");
                echo BlockMain($userinfo, $menu, $content);
            }

            else

            if (strlen($newpass) < 6)
            {
                $userinfo = "";
                $menu = BlockMenu(SCREEN_AUTH, 2);
                $content = BlockRegForm("", "", "������ ������ ���� �� ������ ����� ��������");
                echo BlockMain($userinfo, $menu, $content);
            }

            else

            if ($_POST['captcha'] == '')
            {
                $userinfo = "";
                $menu = BlockMenu(SCREEN_AUTH, 2);
                $content = BlockRegForm("", "", "������� ��� � ��������");
                echo BlockMain($userinfo, $menu, $content);
            }

            else

            if (!($_SESSION['captcha_keystring'] == $_POST['captcha']))
            {
                $userinfo = "";
                $menu = BlockMenu(SCREEN_AUTH, 2);
                $content = BlockRegForm("", "", "������� ��� ��� ���");
                echo BlockMain($userinfo, $menu, $content);
            }

            else

            // ������������!
            {
                require_once 'mysql.php';
                require_once 'common.php';

                $hash = md5($newpass);
                $key = GenerateKey();

                if (!AddActivate($newlogin, $hash, $key))
                {
                    $userinfo = "";
                    $menu = BlockMenu(SCREEN_AUTH, 2);
                    $content = BlockRegForm("", "", "������ ����������� (MySQL)");// . mysqli_error(sqlLink()));
                    echo BlockMain($userinfo, $menu, $content);
                }
                else
                {
                    if (!SendRegMail($newlogin, $key))
                    {
                        $userinfo = "";
                        $menu = BlockMenu(SCREEN_AUTH, 2);
                        $content = BlockRegForm("", "", "������ ����������� (mail)");
                        echo BlockMain($userinfo, $menu, $content);
                    }
                    else
                    {
                        $userinfo = "";
                        $menu = BlockMenu(SCREEN_AUTH, 2);
                        $content = BlockRegSucceed($newlogin);
                        echo BlockMain($userinfo, $menu, $content);
                    }
                }
            }
        }
    }

    ?>
