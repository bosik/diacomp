/*
 *  Diacomp - Diabetes analysis & management system
 *  Copyright (C) 2023 Nikita Bosik
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */
package org.bosik.diacomp.android.backend.common.webclient.retrofit;

import java.util.Map;

import okhttp3.ResponseBody;
import retrofit2.Call;
import retrofit2.http.FieldMap;
import retrofit2.http.FormUrlEncoded;
import retrofit2.http.GET;
import retrofit2.http.POST;
import retrofit2.http.PUT;
import retrofit2.http.Url;

public interface DiacompApi
{
	@GET
	Call<ResponseBody> get(@Url String url);

	@POST
	@FormUrlEncoded
	Call<ResponseBody> post(@Url String url, @FieldMap Map<String, String> params);

	@PUT
	@FormUrlEncoded
	Call<ResponseBody> put(@Url String url, @FieldMap Map<String, String> params);

	@GET("api/system/time")
	Call<ResponseBody> getCurrentTimeUTC();
}
