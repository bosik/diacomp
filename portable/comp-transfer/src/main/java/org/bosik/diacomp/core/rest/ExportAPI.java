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

public interface ExportAPI
{
	String JSON_DIARY       = "diary.json";
	String JSON_FOODBASE    = "foodbase.json";
	String JSON_DISHBASE    = "dishbase.json";
	String JSON_PREFERENCES = "preferences.json";

	String PLAIN_DIARY       = "diary.txt";
	String PLAIN_FOODBASE    = "foodbase.txt";
	String PLAIN_DISHBASE    = "dishbase.txt";
	String PLAIN_PREFERENCES = "preferences.txt";
}