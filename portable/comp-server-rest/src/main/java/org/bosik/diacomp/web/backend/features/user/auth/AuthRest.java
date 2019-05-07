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
package org.bosik.diacomp.web.backend.features.user.auth;

import org.bosik.diacomp.core.rest.ResponseBuilder;
import org.bosik.diacomp.core.services.exceptions.NotAuthorizedException;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpSession;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

@RestController
@RequestMapping("/auth")
public class AuthRest extends UserRest
{
	public static final int API_CURRENT = 20;
	public static final int API_LEGACY  = 19;

	@Autowired
	private AuthProvider authProvider;

	@PostMapping(path = "/login"
			//, consumes = MediaType.APPLICATION_FORM_URLENCODED
			//, produces = MediaType.APPLICATION_JSON
	)
	public ResponseEntity login(
			@RequestParam("login") String login,
			@RequestParam("pass") String pass,
			@RequestParam("api") int apiVersion)
	{
		try
		{
			// check the values
			if (apiVersion < API_LEGACY)
			{
				String msg = String
						.format("API %d is unsupported. The latest API: %d. Legacy API: %d.", apiVersion, API_CURRENT, API_LEGACY);
				String resp = ResponseBuilder.build(ResponseBuilder.CODE_UNSUPPORTED_API, msg);
				return ResponseEntity.ok(resp);
			}

			if (apiVersion < API_CURRENT)
			{
				String msg = String
						.format("API %d is still supported, but deprecated. The latest API: %d. Legacy API: %d.", apiVersion, API_CURRENT,
								API_LEGACY);
				String resp = ResponseBuilder.build(ResponseBuilder.CODE_DEPRECATED_API, msg);
				return ResponseEntity.ok(resp);
			}

			// authentication
			Authentication authentication = authProvider.authenticate(new UsernamePasswordAuthenticationToken(login, pass));

			// setting context
			SecurityContext context = SecurityContextHolder.getContext();
			context.setAuthentication(authentication);
			getSession().setAttribute("Login", authentication.getPrincipal()); // for Tomcat to show in "Guessed User name" nicely

			// Do not pass any data in the body in order to store the same session-id
			return ResponseEntity.ok().build();
		}
		catch (AuthenticationException e)
		{
			// Do not reset session flag here: anyone can reset your session otherwise
			String entity = ResponseBuilder.build(ResponseBuilder.CODE_BADCREDENTIALS, "Bad username/password");
			return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(entity);
		}
	}

	@POST
	@Path("login/json")
	@Produces(MediaType.APPLICATION_JSON)
	public Response loginJsonCookie(String data)
	{
		try
		{
			// check if all params are presented

			if (data == null || data.isEmpty())
			{
				return Response.status(Status.BAD_REQUEST).entity("Provide JSON body with credentials info").build();
			}

			if (data.length() > 512)
			{
				return Response.status(Status.BAD_REQUEST).entity("Credentials object is too big").build();
			}

			JSONObject json = new JSONObject(data);

			final String KEY_USERNAME = "username";
			final String KEY_PASSWORD = "password";

			if (!json.has(KEY_USERNAME))
			{
				return Response.status(Status.BAD_REQUEST).entity("Credentials: missing field '" + KEY_USERNAME + "'").build();
			}

			if (!json.has(KEY_PASSWORD))
			{
				return Response.status(Status.BAD_REQUEST).entity("Credentials: missing field '" + KEY_PASSWORD + "'").build();
			}

			String username = json.getString(KEY_USERNAME);
			String password = json.getString(KEY_PASSWORD);

			// check credentials
			Authentication authentication = authProvider.authenticate(new UsernamePasswordAuthenticationToken(username, password));
			SecurityContext context = SecurityContextHolder.getContext();
			context.setAuthentication(authentication);

			getSession().setAttribute("Login", authentication.getPrincipal()); // for Tomcat to show in "Guessed User name" nicely

			// Do not pass any data in the body in order to store the same session-id
			return Response.ok().build();
		}
		catch (AuthenticationException e)
		{
			// Do not reset session flag here: anyone can reset your session otherwise
			String entity = ResponseBuilder.build(ResponseBuilder.CODE_BADCREDENTIALS, "Bad username/password");
			return Response.status(Status.UNAUTHORIZED).entity(entity).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	//	@POST
	//	@Path("login/json/token")
	//	@Produces(MediaType.APPLICATION_JSON)
	//	public Response loginJsonToken(String data)
	//	{
	//		try
	//		{
	//			// check if all params are presented
	//
	//			if (data == null || data.isEmpty())
	//			{
	//				return Response.status(Status.BAD_REQUEST).entity("Provide body with credentials info").build();
	//			}
	//
	//			if (data.length() > 512)
	//			{
	//				return Response.status(Status.BAD_REQUEST).entity("Credentials object is too big").build();
	//			}
	//
	//			JSONObject json = new JSONObject(data);
	//
	//			final String KEY_USERNAME = "username";
	//			final String KEY_PASSWORD = "password";
	//
	//			if (!json.has(KEY_USERNAME))
	//			{
	//				return Response.status(Status.BAD_REQUEST).entity("Credentials: missing field 'username'").build();
	//			}
	//
	//			if (!json.has(KEY_PASSWORD))
	//			{
	//				return Response.status(Status.BAD_REQUEST).entity("Credentials: missing field 'password'").build();
	//			}
	//
	//			String username = json.getString(KEY_USERNAME);
	//			String password = json.getString(KEY_PASSWORD);
	//
	//			// check credentials
	//			String token = tokenService.createToken(username, password);
	//
	//			// build token
	//			JSONObject response = new JSONObject();
	//			response.put("token", token);
	//
	//			return Response.ok(response.toString()).build();
	//		}
	//		catch (AuthenticationException e)
	//		{
	//			// Do not reset session flag here: anyone can reset your session otherwise
	//			String entity = ResponseBuilder.build(ResponseBuilder.CODE_BADCREDENTIALS, "Bad username/password");
	//			return Response.status(Status.UNAUTHORIZED).entity(entity).build();
	//		}
	//		catch (Exception e)
	//		{
	//			e.printStackTrace();
	//			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
	//		}
	//	}

	private static HttpSession getSession()
	{
		ServletRequestAttributes attr = (ServletRequestAttributes) RequestContextHolder.currentRequestAttributes();
		return attr.getRequest().getSession(true);
	}

	@SuppressWarnings("static-method")
	@GET
	@Path("logout")
	@Produces(MediaType.APPLICATION_JSON)
	public Response logout()
	{
		try
		{
			SecurityContextHolder.clearContext();
			String entity = "Logged out OK";
			return Response.ok(entity).build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}

	@GET
	@Path("check")
	@Produces(MediaType.TEXT_PLAIN)
	public Response check()
	{
		try
		{
			getUserId();
			return Response.ok("Authorized").build();
		}
		catch (AuthenticationException | NotAuthorizedException e)
		{
			return Response.status(Status.UNAUTHORIZED).entity("Unauthorized").build();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return Response.status(Status.INTERNAL_SERVER_ERROR).entity(ResponseBuilder.buildFails()).build();
		}
	}
}
