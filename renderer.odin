package main

import "core:fmt"
import "core:os"
import "core:math"

import vk "vendor:vulkan"

import "shared:stb/stbtt"

Renderer :: struct
{
	curr_job: ^Render_Job,
	jobs: [dynamic]Render_Job,
	window: Window,
	default_font: ^Font,
}

Render_Job :: struct
{
	vertices: [dynamic]Vertex,
	indices : [dynamic]u16,
	font : ^Font,
	clip    : Rect,
	vertex_buffer: VKBuffer,
	index_buffer: VKBuffer,
}

renderer_destroy :: proc(ctx: ^Context, r: ^Renderer)
{
	for j in &r.jobs
	{
		delete(j.vertices);
		delete(j.indices);
		destroy_vkbuffer(ctx, j.vertex_buffer);
		destroy_vkbuffer(ctx, j.index_buffer);
		j = {};
	}
	clear(&r.jobs);
	r.curr_job= nil;
}

renderer_start_frame :: proc(ctx: ^Context, r: ^Renderer, font: ^Font, window: Window)
{
	renderer_destroy(ctx, r);
	renderer_set_clip(r, {{0, 0}, {cast(f32)window.res.x, cast(f32)window.res.y}});
	renderer_set_font(r, font);
}

renderer_end_frame :: proc(ctx: ^Context, r: ^Renderer)
{
	for j in &r.jobs
	{
		j.vertex_buffer = create_vertex_buffer(ctx, j.vertices[:]);
		j.index_buffer = create_index_buffer(ctx, j.indices[:]);
	}
}

renderer_record :: proc(using ctx: ^Context, r: ^Renderer, buffer: vk.CommandBuffer, image_index: u32)
{
	begin_info: vk.CommandBufferBeginInfo;
	begin_info.sType = .COMMAND_BUFFER_BEGIN_INFO;
	begin_info.flags = {};
	begin_info.pInheritanceInfo = nil;
	
	if res := vk.BeginCommandBuffer(buffer,  &begin_info); res != .SUCCESS
	{
		fmt.eprintf("Error: Failed to begin recording command buffer!\n");
		os.exit(1);
	}
	
	render_pass_info: vk.RenderPassBeginInfo;
	render_pass_info.sType = .RENDER_PASS_BEGIN_INFO;
	render_pass_info.renderPass = pipeline.render_pass;
	render_pass_info.framebuffer = swap_chain.framebuffers[image_index];
	render_pass_info.renderArea.offset = {0, 0};
	render_pass_info.renderArea.extent = swap_chain.extent;
	
	
	clear_values := [2]vk.ClearValue{
		0 = {color={float32 = [4]f32{0.0, 0.0, 0.0, 1.0}}},
		1 = {depthStencil = {1.0, 0}},
	};
	
	render_pass_info.clearValueCount = len(clear_values);
	render_pass_info.pClearValues = &clear_values[0];
	
	for j in &r.jobs
	{
		vk.CmdBeginRenderPass(buffer, &render_pass_info, .INLINE);
		
		vk.CmdBindPipeline(buffer, .GRAPHICS, pipeline.handle);
		
		vertex_buffers := [?]vk.Buffer{j.vertex_buffer.buffer};
		offsets := [?]vk.DeviceSize{0};
		vk.CmdBindVertexBuffers(buffer, 0, 1, &vertex_buffers[0], &offsets[0]);
		vk.CmdBindIndexBuffer(buffer, j.index_buffer.buffer, 0, .UINT16);
		
		viewport: vk.Viewport;
		viewport.x = 0.0;
		viewport.y = 0.0;
		viewport.width = f32(swap_chain.extent.width);
		viewport.height = f32(swap_chain.extent.height);
		viewport.minDepth = 0.0;
		viewport.maxDepth = 1.0;
		vk.CmdSetViewport(buffer, 0, 1, &viewport);
		
		scissor: vk.Rect2D;
		scissor.offset = {cast(i32)j.clip.tl.x, cast(i32)j.clip.tl.y};
		scissor.extent = {cast(u32)(j.clip.br.x - j.clip.tl.x), cast(u32)(j.clip.br.y - j.clip.tl.y)};
		vk.CmdSetScissor(buffer, 0, 1, &scissor);
		
		vk.CmdBindDescriptorSets(buffer, .GRAPHICS, pipeline.layout, 0, 1, &descriptor_sets[curr_frame], 0, nil);
		vk.CmdDrawIndexed(buffer, cast(u32)j.index_buffer.length, 1, 0, 0, 0);
		
		vk.CmdEndRenderPass(buffer);
	}
	
	if res := vk.EndCommandBuffer(buffer); res != .SUCCESS
	{
		fmt.eprintf("Error: Failed to record command buffer!\n");
		os.exit(1);
	}
}

renderer_new_job :: #force_inline proc(using r: ^Renderer)
{
	append(&jobs, Render_Job{font=default_font, clip={{0, 0}, {cast(f32)window.res.x, cast(f32)window.res.y}}});
	curr_job = &jobs[len(jobs)-1];
}

renderer_set_font :: proc(using r: ^Renderer, font: ^Font)
{
	if curr_job == nil || (len(curr_job.vertices) != 0 && curr_job.font != font)
	{
		renderer_new_job(r);
	}
	curr_job.font = font;
}

renderer_set_clip :: proc(using r: ^Renderer, clip: Rect)
{
	if curr_job == nil || (len(curr_job.vertices) != 0 && curr_job.clip != clip)
	{
		renderer_new_job(r);
	}
	curr_job.clip = clip;
}

renderer_text_width :: proc(using r: ^Renderer, text: string) -> f32
{
	/*
		if curr_job.font == nil
		{
			fmt.eprintf("Error: Renderer has no font\n");
			os.exit(1);
		}
		
		font := curr_job.font;
		w := f32(0.0);
		pos := [2]f32{0, 0};
		for c in text
		{
			stbtt.get_baked_quad(font.glyphs, cast(int)font.image.dim.x, cast(int)font.image.dim.y, cast(int)c, &pos, true);
		}
	*/
	return 0;
}

renderer_draw_text_ft :: proc(using r: ^Renderer, text: string, pos: [2]f32, color: [4]f32)
{
	if curr_job.font == nil
	{
		fmt.eprintf("Error: Renderer has no font\n");
		os.exit(1);
	}
	
	// fmt.println("TEST");
	font := curr_job.font;
	pos := [2]u32{u32(math.round(pos.x)), u32(math.round(pos.y))};
	color := [4]f32{1, 0, 0, 1};
	for c in text
	{
		//q := stbtt.get_baked_quad(font.glyphs, cast(int)font.image.dim.x, cast(int)font.image.dim.y, cast(int)c, &pos, true);
		g := font.glyphs[c];
		x0 := pos.x + g.xoff;
		x1 := x0 + (g.x1 - g.x0);
		y0 := pos.y - g.yoff;
		y1 := y0 + (g.y1 - g.y0);
		
		s0 := f32(g.x0) / f32(font.image.dim.x);
		s1 := f32(g.x1) / f32(font.image.dim.x);
		t0 := (f32(g.y0) / f32(font.image.dim.y));
		t1 := (f32(g.y1) / f32(font.image.dim.y));
		
		vertices := [4]Vertex{
			{{f32(x0), f32(y0), 0.0}, color, {s0, t0}},
			{{f32(x0), f32(y1), 0.0}, color, {s0, t1}},
			{{f32(x1), f32(y1), 0.0}, color, {s1, t1}},
			{{f32(x1), f32(y0), 0.0}, color, {s1, t0}},
		};
		
		indices := [6]u16{
			0, 1, 2,
			2, 3, 0,
		};
		
		indices += cast(u16)len(curr_job.vertices);
		
		append(&curr_job.vertices, ..vertices[:]);
		append(&curr_job.indices,  ..indices [:]);
		
		//pos.x = math.floor(pos.x + x_advance);
		pos.x += g.xadvance;
	}
}

renderer_draw_rect :: proc(using r: ^Renderer, rect: Rect, color: [4]f32)
{
	if curr_job.font == nil
	{
		fmt.eprintf("Error: Renderer has no font\n");
		os.exit(1);
	}
	
	font := curr_job.font;
	
	vertices := [4]Vertex{
		{{rect.tl.x, rect.tl.y, 0.0}, color, font.id_uv},
		{{rect.tl.x, rect.br.y, 0.0}, color, font.id_uv},
		{{rect.br.x, rect.br.y, 0.0}, color, font.id_uv},
		{{rect.br.x, rect.tl.y, 0.0}, color, font.id_uv},
	};
	
	indices := [6]u16{
		0, 1, 2,
		2, 3, 0,
	};
	
	indices += cast(u16)len(curr_job.vertices);
	
	append(&curr_job.vertices, ..vertices[:]);
	append(&curr_job.indices,  ..indices [:]);
}