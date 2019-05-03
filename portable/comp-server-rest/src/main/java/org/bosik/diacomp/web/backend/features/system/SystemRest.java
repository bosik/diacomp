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
package org.bosik.diacomp.web.backend.features.system;

import com.fasterxml.jackson.annotation.JsonProperty;
import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.rest.StdResponse;
import org.bosik.diacomp.core.utils.Utils;
import org.bosik.diacomp.web.backend.common.Config;
import org.bosik.diacomp.web.backend.features.user.auth.AuthRest;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.ws.rs.core.MediaType;
import java.util.Date;

@RestController
public class SystemRest
{
	@GetMapping(produces = MediaType.TEXT_PLAIN)
	public String welcome()
	{
		String buildTime = Config.get(Config.KEY_BUILD_TIME);
		String buildCommit = Config.get(Config.KEY_BUILD_COMMIT);

		StringBuilder s = new StringBuilder();
		s.append("Diacomp REST API is up\n");
		s.append("Built ").append(buildTime).append(" / ").append(buildCommit);

		return s.toString();
	}

	@GetMapping(path = "/system/info", produces = MediaType.APPLICATION_JSON)
	public ApiInfo getAPIVersion()
	{
		return new ApiInfo()
		{{
			setCode(ResponseBuilder.CODE_OK);
			setInfo(new ApiVersionInfo()
			{{
				setCurrentVersion(AuthRest.API_CURRENT);
				setSupportedVersion(AuthRest.API_LEGACY);
				setBuild(Config.get(Config.KEY_BUILD_TIME));
			}});
		}};
	}

	@GetMapping(path = "/system/time", produces = MediaType.TEXT_PLAIN)
	public String getTime()
	{
		return Utils.formatTimeUTC(new Date());
	}

	private static class ApiInfo
	{
		@JsonProperty(StdResponse.TAG_CODE)
		private int code;

		@JsonProperty(StdResponse.TAG_RESPONSE)
		private ApiVersionInfo info;

		public int getCode()
		{
			return code;
		}

		public void setCode(int code)
		{
			this.code = code;
		}

		public ApiVersionInfo getInfo()
		{
			return info;
		}

		public void setInfo(ApiVersionInfo info)
		{
			this.info = info;
		}
	}

	private static class ApiVersionInfo
	{
		@JsonProperty("current")
		private int currentVersion;

		@JsonProperty("support")
		private int supportedVersion;

		@JsonProperty("build")
		private String build;

		public int getCurrentVersion()
		{
			return currentVersion;
		}

		public void setCurrentVersion(int currentVersion)
		{
			this.currentVersion = currentVersion;
		}

		public int getSupportedVersion()
		{
			return supportedVersion;
		}

		public void setSupportedVersion(int supportedVersion)
		{
			this.supportedVersion = supportedVersion;
		}

		public String getBuild()
		{
			return build;
		}

		public void setBuild(String build)
		{
			this.build = build;
		}
	}
}
