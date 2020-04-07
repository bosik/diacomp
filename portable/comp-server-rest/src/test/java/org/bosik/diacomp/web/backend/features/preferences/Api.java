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
package org.bosik.diacomp.web.backend.features.preferences;

public class Api
{
	public static class Preferences
	{
		public static final String BASE_URL = "/preferences";

		public static class GetAll
		{
			public static final String URL = BASE_URL;
		}

		public static class GetString
		{
			public static final String URL = BASE_URL;
		}

		public static class Hash
		{
			public static final String URL = BASE_URL + "/hash";
		}

		public static class Save
		{
			public static final String URL         = BASE_URL;
			public static final String PARAM_DATA  = "data";
			public static final String RESPONSE_OK = "Saved OK";
		}
	}
}
