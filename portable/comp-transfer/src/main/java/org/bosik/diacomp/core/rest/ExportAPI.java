/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.core.rest;

public final class ExportAPI
{
	public static final String	ANDROID_JSON_DIARY			= "diary.json";
	public static final String	ANDROID_JSON_FOODBASE		= "foodbase.json";
	public static final String	ANDROID_JSON_DISHBASE		= "dishbase.json";
	public static final String	ANDROID_JSON_PREFERENCES	= "preferences.json";

	public static final String	ANDROID_PLAIN_DIARY			= "diary.txt";
	public static final String	ANDROID_PLAIN_FOODBASE		= "foodbase.txt";
	public static final String	ANDROID_PLAIN_DISHBASE		= "dishbase.txt";
	public static final String	ANDROID_PLAIN_PREFERENCES	= "preferences.txt";

	private ExportAPI()
	{
	}
}