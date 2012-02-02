<?php
/*****************************************************************************
 *                                                                           *
 * Shop-Script FREE                                                          *
 * Copyright (c) 2006 Articus consulting group. All rights reserved.         *
 *                                                                           *
 ****************************************************************************/



	//this file indicates listing of all available languages

class Language
{
	var $id; 
	var $description; //language name
	var $filename; //language PHP constants file
	var $template; //template filename
}

	//a list of languages
	$lang_list = array();

	//to add new languages add similiar structures

	$lang_list[0] = new Language();
	$lang_list[0]->id = "ru";
	$lang_list[0]->description = "Русский";
	$lang_list[0]->filename = "russian.php";
	$lang_list[0]->template_path = "./templates/tmpl1/";

	$lang_list[1] = new Language();
	$lang_list[1]->id = "de";
	$lang_list[1]->description = "Deutsch";
	$lang_list[1]->filename = "deutsch.php";
	$lang_list[1]->template_path = "./templates/tmpl_de/";

	$lang_list[2] = new Language();
	$lang_list[2]->id = "en";
	$lang_list[2]->description = "English";
	$lang_list[2]->filename = "english.php";
	$lang_list[2]->template_path = "./templates/tmpl_en/";


?>
