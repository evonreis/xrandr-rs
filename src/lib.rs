#![warn(clippy::cargo)]

//! This crate aims to provide safe bindings to libxrandr. It currently supports reading most
//! monitor properties.
//!
//! ```
//! # use xrandr::XHandle;
//! let monitors = XHandle::open()?
//!     .monitors()?;
//!
//! println!("{:#?}", monitors);
//! # Ok::<_, xrandr::XrandrError>(())
//! ```
//!
//! Example output on my laptop:
//! ```rust,ignore
//! [
//!     Monitor {
//!         name: "eDP-1",
//!         is_primary: true,
//!         is_automatic: true,
//!         x: 0,
//!         y: 0,
//!         width_px: 1920,
//!         height_px: 1080,
//!         width_mm: 344,
//!         height_mm: 194,
//!         outputs: [
//!             Output {
//!                 xid: 66,
//!                 name: "eDP-1",
//!                 properties: {
//!                     "EDID": Property {
//!                         name: "EDID",
//!                         value: Edid([ 0, 255, 255, /* ... */ 80, 68, 49, 0, 62, ]),
//!                         values: None,
//!                         is_immutable: false,
//!                         is_pending: false,
//!                     },
//!                     "scaling mode": Property {
//!                         name: "scaling mode",
//!                         value: Atom("Full aspect"),
//!                         values: Some(
//!                             Supported(
//!                                 Atom(
//!                                     [
//!                                         "Full",
//!                                         "Center",
//!                                         "Full aspect",
//!                                     ],
//!                                 ),
//!                             ),
//!                         ),
//!                         is_immutable: false,
//!                         is_pending: false,
//!                     },
//!                     /* ... */
//!                     "non-desktop": Property {
//!                         name: "non-desktop",
//!                         value: Integer32([0]),
//!                         values: Some(
//!                             Range(
//!                                 Integer8(
//!                                     [
//!                                         Range {
//!                                             lower: 0,
//!                                             upper: 1,
//!                                         },
//!                                     ],
//!                                 ),
//!                             ),
//!                         ),
//!                         is_immutable: true,
//!                         is_pending: false,
//!                     },
//!                 },
//!             },
//!         ],
//!     },
//! ]
//! ```
#![feature(cstr_from_bytes_until_nul)]
use std::ffi::{CStr};
use std::fmt::{Debug};
use std::os::raw::c_ulong;
use std::{ptr, slice};
use std::convert::TryFrom;

pub use indexmap;
#[cfg(feature = "serialize")]
use serde::{Deserialize, Serialize};
use thiserror::Error;
use x11::{xlib, xrandr};

pub use output::{
    property::{Property, PropertyValue, PropertyValues, Range, Ranges, Supported},
    Output,
};

mod output;

type HandleSys = ptr::NonNull<xlib::Display>;

#[derive(Debug)]
pub struct XHandle {
    sys: HandleSys,
}

impl XHandle {
    pub fn open() -> Result<Self, XrandrError> {
        let sys = ptr::NonNull::new(unsafe { xlib::XOpenDisplay(ptr::null()) })
            .ok_or(XrandrError::Open)?;

        Ok(Self { sys })
    }

    /// List every monitor
    pub fn monitors(&mut self) -> Result<Vec<Monitor>, XrandrError> {
        let mut count = 0;
        let infos =
            unsafe { xrandr::XRRGetMonitors(self.sys.as_ptr(), self.root(), 0, &mut count) };
        if count == -1 {
            return Err(XrandrError::GetMonitors);
        }
        let count = count as usize;
        let data = ptr::NonNull::new(infos).expect("Succeeded, so non-null");

        let list = unsafe { slice::from_raw_parts_mut(data.as_ptr(), count) }
            .iter()
            .map(|sys| {
                let outputs = unsafe { Output::from_list(self, sys.outputs, sys.noutput) }?;

                Ok(Monitor {
                    name: atom_name(&mut self.sys, sys.name)?,
                    is_primary: real_bool(sys.primary),
                    is_automatic: real_bool(sys.automatic),
                    x: sys.x,
                    y: sys.y,
                    rotate: 1,
                    width_px: sys.width,
                    height_px: sys.height,
                    width_mm: sys.mwidth,
                    height_mm: sys.mheight,
                    outputs,
                })
            })
            .collect::<Result<_, _>>()?;

        unsafe {
            xrandr::XRRFreeMonitors(data.as_ptr());
        }

        Ok(list)
    }

    pub fn set_monitor(&mut self, monitor: & mut Monitor, refresh: f64) -> Result<(), XrandrError> {

        let _mode_info = self.get_mode_info(monitor)?;
        let best_mode_id = self.find_best_mode(monitor.width_px, monitor.height_px, refresh)?;

        let res_ptr = unsafe{ xrandr::XRRGetScreenResources(self.sys.as_ptr(), self.root()) };

        let crtc = monitor.outputs[0].crtc;
        let crtc_info = unsafe{ xrandr::XRRGetCrtcInfo(self.sys.as_ptr(), res_ptr, crtc) };
        let crtc_ref = unsafe{ &(*crtc_info) };

        let _best_mode_info = self.get_mode_info_by_id(best_mode_id);
        let _crtc_mode_info = self.get_mode_info_by_id(crtc_ref.mode);

        // dbg!(best_mode_info);
        // dbg!(crtc_mode_info);
        //
        // dbg!(best_mode_id);
        // dbg!(crtc_ref.mode);
        unsafe{ xrandr::XRRSetCrtcConfig(self.sys.as_ptr(), res_ptr, crtc,
                                         crtc_ref.timestamp, monitor.x, monitor.y,
                                        best_mode_id,
                                        //crtc_ref.mode,
                                        monitor.rotate, crtc_ref.outputs,
                                        crtc_ref.noutput); }

        unsafe{ xrandr::XRRFreeScreenResources( res_ptr.cast()) ; }
        drop(res_ptr);

        drop(crtc_ref);
        unsafe{ xrandr::XRRFreeCrtcInfo(crtc_info.cast()); }
        drop(crtc_info);

        Ok(())
    }

    pub fn get_mode_info(&mut self, monitor: & Monitor) -> Result<ModeInfo, XrandrError> {
         let mode_id = monitor.get_mode_id();
         let mode_info =  self.get_mode_info_by_id(mode_id);

         mode_info
    }

    pub fn get_mode_info_by_id(&mut self, mode_id: u64) -> Result<ModeInfo, XrandrError> {
        let res_ptr = unsafe{ xrandr::XRRGetScreenResources(self.sys.as_ptr(), self.root()) };
        let res = unsafe{ &*res_ptr };

        let num_modes = usize::try_from(res.nmode).unwrap();

        let mut found_node: Option<ModeInfo> = None;

        for mode_index in 0..num_modes {
            let mode_ptr = unsafe{ res.modes.add(mode_index) };
            let mode = unsafe{ &*mode_ptr };

            if mode.id == mode_id {
                found_node = Some(ModeInfo::from_xrr_mode_info(mode));
                break;
            }
        }

        drop(res);
        unsafe{ xrandr::XRRFreeScreenResources( res_ptr.cast()) ; }
        drop(res_ptr);

        found_node.ok_or(XrandrError::GetModeById(mode_id))
    }

    /// returns mode id for best mode
    /// has to have correct resolution, and nearest refresh rate.
    pub fn find_best_mode(&mut self, width: i32, height: i32, refresh: f64) -> Result<u64, XrandrError> {
        let res_ptr = unsafe{ xrandr::XRRGetScreenResources(self.sys.as_ptr(), self.root()) };
        let res = unsafe{ &*res_ptr };

        let num_modes = usize::try_from(res.nmode).unwrap();

        let mut best_mode: Option<u64> = None;
        let mut best_dist = 1e100;
        let mut _best_mode_info = unsafe{ &* res.modes };

        for mode_index in 0..num_modes {
            let mode_ptr = unsafe{ res.modes.add(mode_index) };
            let mode = unsafe{ &*mode_ptr };

            let check_mode = ModeInfo::from_xrr_mode_info(mode);

            let dist = f64::abs(check_mode.refresh() - refresh);

            if check_mode.width == u32::try_from(width).unwrap() && check_mode.height == u32::try_from(height).unwrap() {

                println!("mod_id={}\trefresh={} wanted={}", check_mode.id, check_mode.refresh(), refresh);

                if dist < best_dist {
                    best_dist = dist;
                    best_mode = Some(check_mode.id);
                    _best_mode_info = mode;
                }
            }
        }

        drop(res);
        unsafe{ xrandr::XRRFreeScreenResources( res_ptr.cast()) ; }
        drop(res_ptr);

        dbg!(best_mode);
        best_mode.ok_or(XrandrError::FindBestMode(width, height, refresh))
    }

    pub fn set_screen_size(&mut self, width: i32, height: i32, width_mm: i32, height_mm: i32) {
        unsafe{ xrandr::XRRSetScreenSize(self.sys.as_ptr(), self.root(), width, height,
                height_mm, width_mm); }
    }

    /// List every monitor's outputs
    pub fn all_outputs(&mut self) -> Result<Vec<Output>, XrandrError> {
        let res = self.res()?;
        unsafe { Output::from_list(self, res.outputs, res.noutput) }
    }

    fn res<'r, 'h>(&'h mut self) -> Result<&'r mut xrandr::XRRScreenResources, XrandrError>
    where
        'r: 'h,
    {
        let res = unsafe {
            ptr::NonNull::new(xrandr::XRRGetScreenResources(
                self.sys.as_ptr(),
                self.root(),
            ))
            .ok_or(XrandrError::GetResources)?
            .as_mut()
        };
        Ok(res)
    }

    fn root(&mut self) -> c_ulong {
        unsafe { xlib::XDefaultRootWindow(self.sys.as_ptr()) }
    }
}

impl Drop for XHandle {
    fn drop(&mut self) {
        unsafe {
            xlib::XCloseDisplay(self.sys.as_ptr());
        }
    }
}

#[derive(Debug)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
pub struct Monitor {
    pub name: String,
    pub is_primary: bool,
    pub is_automatic: bool,
    pub x: i32,
    pub y: i32,
    pub width_px: i32,
    pub height_px: i32,
    pub width_mm: i32,
    pub height_mm: i32,
    pub rotate: u16,
    /// An Output describes an actual physical monitor or display. A [`Monitor`]
    /// can have more than one output.
    pub outputs: Vec<Output>,
}

impl Monitor {
    pub fn to_rr_monitor_info(&self, name_atom: c_ulong, rr_mon_info: * mut xrandr::XRRMonitorInfo) {
        let mut out_ref = unsafe{ &mut *rr_mon_info };

        out_ref.name = name_atom;
        out_ref.primary = xlib::Bool::from(self.is_primary);
        out_ref.automatic = xlib::Bool::from(self.is_automatic);
        out_ref.noutput = i32::try_from(self.outputs.len()).unwrap();
        out_ref.x = self.x;
        out_ref.y = self.y;
        out_ref.width = self.width_px;
        out_ref.height = self.height_px;
        out_ref.mwidth = self.width_mm;
        out_ref.mheight = self.height_mm;

        for (i, output) in self.outputs.iter().enumerate() {
            unsafe{ *(out_ref.outputs.add(i)) = output.xid }
        }
    }

    pub fn get_rotate(&self) -> xrandr::Rotation {
        let mut xh = XHandle::open().unwrap();

        let res = unsafe{ xrandr::XRRGetScreenResources(xh.sys.as_ptr(), xh.root()) };

        let crtc = self.outputs[0].crtc;

        let crtc_info = unsafe{ xrandr::XRRGetCrtcInfo(xh.sys.as_ptr(), res, crtc) };

        let crtc_ref = unsafe{ &(*crtc_info) };

        let rotate = crtc_ref.rotations;

        unsafe{ xrandr::XRRFreeCrtcInfo(crtc_info.cast()); }

        unsafe{ xrandr::XRRFreeScreenResources( res.cast()) ; }

        rotate

    }

    pub fn get_mode_id(&self) -> u64 {

        let mut xh = XHandle::open().unwrap();
        let res = unsafe{ xrandr::XRRGetScreenResources(xh.sys.as_ptr(), xh.root()) };

        let crtc = self.outputs[0].crtc;
        let crtc_info = unsafe{ xrandr::XRRGetCrtcInfo(xh.sys.as_ptr(), res, crtc) };
        let crtc_ref = unsafe{ &(*crtc_info) };

        let mode_id = crtc_ref.mode;

        drop(crtc_ref);
        unsafe{ xrandr::XRRFreeCrtcInfo(crtc_info.cast()); }
        drop(crtc_info);

        unsafe{ xrandr::XRRFreeScreenResources( res.cast()) ; }
        drop(res);

        mode_id
    }

}

#[derive(Debug)]
pub struct ModeInfo {
    pub id: u64,
    pub width: u32,
    pub height: u32,
    pub dot_clock: u64,
    pub h_sync_start: u32,
    pub h_sync_end: u32,
    pub h_total: u32,
    pub h_skew: u32,
    pub v_sync_start: u32,
    pub v_sync_end: u32,
    pub v_total: u32,
    pub name: String,
    pub mode_flags: u64,
}

impl ModeInfo {
    pub fn from_xrr_mode_info(other: * const xrandr::XRRModeInfo) -> ModeInfo {

        let p = unsafe{ &*other };

        let name: String = String::new();

        ModeInfo {
            id: p.id,
            width: p.width,
            height: p.height,
            dot_clock: p.dotClock,
            h_sync_start: p.hSyncStart,
            h_sync_end: p.hSyncEnd,
            h_total: p.hTotal,
            h_skew: p.hSkew,
            v_sync_start: p.vSyncStart,
            v_sync_end: p.vSyncEnd,
            v_total: p.vTotal,
            name: name.clone(),
            mode_flags: p.modeFlags,
        }

    }

    pub fn refresh(& self) -> f64 {

        let clock: f64 = self.dot_clock as f64;
        let mut vert: f64 = self.v_total as f64;
        let horiz: f64 = f64::try_from(self.h_total).unwrap();

        if (self.mode_flags & u64::try_from(xrandr::RR_Interlace).unwrap()) != 0 {
            vert = vert / 2.0;
        }

        if (self.mode_flags & u64::try_from(xrandr::RR_DoubleScan).unwrap()) != 0 {
            vert = vert * 2.0;
        }

        clock / (vert * horiz)
    }
}

fn real_bool(sys: xlib::Bool) -> bool {
    assert!(sys == 0 || sys == 1);
    sys == 1
}

fn atom_name(handle: &mut HandleSys, atom: xlib::Atom) -> Result<String, XrandrError> {
    let chars = ptr::NonNull::new(unsafe { xlib::XGetAtomName(handle.as_ptr(), atom) })
        .ok_or(XrandrError::GetAtomName(atom))?;

    let name = unsafe { CStr::from_ptr(chars.as_ptr()) }
        .to_string_lossy()
        .to_string();

    unsafe {
        xlib::XFree(chars.as_ptr().cast());
    }

    Ok(name)
}

#[derive(Error, Debug)]
pub enum XrandrError {
    #[error("Failed to open connection to x11. Check out DISPLAY environment variable.")]
    Open,
    #[error("Call to XRRGetMonitors failed.")]
    GetMonitors,
    #[error("Call to XRRGetScreenResources for XRRDefaultRootWindow failed")]
    GetResources,
    #[error("Call to XRRGetOutputInfo for output with xid {0} failed")]
    GetOutputInfo(xlib::XID),
    #[error("Failed to get the properties of output with xid {0}")]
    GetOutputProp(xlib::XID),
    #[error("Failed to find name of atom {0}")]
    GetAtomName(xlib::Atom),
    #[error("Failed to get mode from id {0}")]
    GetModeById(u64),
    #[error("Failed to find mode for {0} x {1}, {2} fps")]
    FindBestMode(i32, i32, f64),
}

#[cfg(test)]
mod tests {
    use super::*;

    fn handle() -> XHandle {
        XHandle::open().unwrap()
    }

    #[test]
    fn can_open() {
        handle();
    }

    #[test]
    fn can_debug_format_monitors() {
        format!("{:#?}", handle().monitors().unwrap());
    }
}
